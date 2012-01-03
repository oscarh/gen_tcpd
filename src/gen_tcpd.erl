%%% ----------------------------------------------------------------------------
%%% Copyright 2008
%%% Martin Carlson, martin@martinc.eu
%%% Oscar Hellström, oscar@hellstrom.st
%%%
%%% All rights reserved
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are
%%% met:
%%%
%%%     * Redistributions of source code must retain the above copyright
%%%       notice, this list of conditions and the following disclaimer.
%%%     * Redistributions in binary form must reproduce the above copyright
%%%       notice, this list of conditions and the following disclaimer in the
%%%       documentation and/or other materials provided with the distribution.
%%%     * The names of its contributors may not be used to endorse or promote
%%%       products derived from this software without specific prior written
%%%       permission.
%%%
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
%%% ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%%% ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
%%% FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
%%% DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%%% SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%%% CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
%%% OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
%%% SUCH DAMAGE.
%%% ----------------------------------------------------------------------------
%%% @copyright 2008 Martin Carlson, Oscar Hellström
%%% @author Martin Carlson <martin@martinc.eu>
%%% @author Oscar Hellström <oscar@hellstrom.st> [http://oscar.hellstrom.st]
%%% @version {@version}, {@date}, {@time}
%%% @doc
%%% The gen_tcpd behaviour is a generic way of accepting TCP connections.
%%%
%%% As with all OTP behaviours it is using a set of callbacks for the
%%% specific parts of the code.
%%% <pre>
%%% gen_tcpd module            Callback module
%%% ---------------            ---------------
%%% gen_tcpd:start_link -----> Module:init/1
%%% -                   -----> Module:handle_connection/2
%%% -                   -----> Module:handle_info/2
%%% -                   -----> Module:terminate/2
%%% </pre>
%%% == Callbacks ==
%%% <pre>
%%% Module:init(Args) -> Result
%%%     Types Args = term()
%%%           Result = {ok, State} | {stop, Reason}
%%% </pre>
%%% After {@link start_link/5} has been called this function is executed
%%% by the new process to initialise its state. If the initialisation is
%%% successful the function should return <code>{ok, State}</code> where
%%% <code>State</code> is the state which will be passed to the client in
%%% in the next callback. <code>Args</code> is the <code>Args</code> passed
%%% to {@link start_link/5}.
%%%
%%% <pre>
%%% Module:handle_connection(Socket, State) -> void()
%%%     Types Socket = {@link socket()}
%%%           State = term()
%%% </pre>
%%% When a TCP / SSL connection is accepted,
%%% `Module:handle_connection(Socket, State)' will be called from a new
%%% process. This process should handle the TCP connection and do whatever
%%% it wants to. When this function returns, the process exits.
%%%
%%% The process which this is called from is linked to the `gen_tcpd'
%%% process. It is allowed to trap exits in the `gen_tcpd' process if
%%% this is wanted. It's also possible to pass the `gen_tcpd' process as
%%% part of the `State' argument to unlink from it.
%%%
%%% It might seem strange that the process is not under some individual
%%% supervisor, but it has been shown that starting children under
%%% supervisors in a vary rapid pace can overload a supervisor and become a
%%% bottleneck in accepting connections.
%%%
%%% <pre>
%%% Module:handle_info(Info, State) -> Result
%%%     Types Info = term()
%%%           State = term()
%%%           Result = {noreply, NewState} | {stop, Reason, NewState}
%%%           NewState = term()
%%%           Reason = term()
%%% </pre>
%%% This function is called if the gen_tcpd process receives any messasge it
%%% dosen't recognise. E.g. <code>{'EXIT', Pid, Reason}</code> messages if
%%% the process is trapping exits.
%%%
%%% <pre>
%%% Module:terminate(Reason, State) -> void()
%%%     Types Reason = term()
%%%           State = term()
%%% </pre>
%%% This function is called by a gen_tcpd when it is about to terminate.
%%%
%%% @type socket()
%%% @end
%%% ----------------------------------------------------------------------------
-module(gen_tcpd).
-behaviour(gen_server).

-export([start_link/5]).
-export([
	send/2,
	recv/2,
	recv/3,
	close/1,
	peername/1,
	port/1,
	sockname/1,
	setopts/2,
	controlling_process/2,
	type/1,
	stop/1
]).
-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	code_change/3,
	terminate/2
]).
-export([init_acceptor/5]).
-export([behaviour_info/1]).

-include("gen_tcpd_types.hrl").

-record(state, {callback, acceptors, socket}).

%% @spec start_link(Callback, CallbackArg, Type, Port, Options) -> {ok, Pid}
%% Callback = atom()
%% CallbackArg = term()
%% Type = tcp | ssl
%% Port = integer()
%% Options = [Opt]
%% Opt = {socket_options, SocketOptions} | {acceptors, Acceptors} |
%%       {ssl_accept_timeout, Timeout}
%% SocketOptions = [SocketOpt]
%% Acceptors = integer()
%% Timeout = infinity | integer()
%% Pid = pid()
%% @doc Starts a gen_tcpd process and links to it.
%% @end
-spec start_link(atom(), term(), ssl | tcp, 0..65535, [{atom(), term()}]) ->
	{ok, pid()} | {error, term()} | ignore.
start_link(Callback, CallbackArg, Type, Port, Options) ->
	Args = [Type, Callback, CallbackArg, Port, Options],
	ok = check_options(Options),
	gen_server:start_link(?MODULE, Args, []).

%% @spec port(Ref) -> Port::integer()
%% Ref = Name | {Name, Node} | {global, GlobalName} | pid()
%% Name = atom()
%% Node = atom()
%% GlobalName = term()
%% @doc
%% Returns the listening port for the gen_tcpd.
%% This is useful if gen server was called with <code>Port</code> =
%% <code>0</code>.
%% @end
-spec port(server_ref()) -> 1..65535.
port(Ref) ->
	gen_server:call(Ref, port).

%% @spec stop(Ref) -> ok
%% Ref = Name | {Name, Node} | {global, GlobalName} | pid()
%% Name = atom()
%% Node = atom()
%% GlobalName = term()
%% @doc
%% Stops the gen_tcpd server and frees the listening port.
%% @end
-spec stop(server_ref()) -> ok.
stop(Ref) ->
	gen_server:cast(Ref, stop).

%% @spec recv(Socket::socket(), Size::integer()) -> Result
%% Result = {ok, Packet} | {error, Reason}
%% Reason = {error, timeout} | {error, posix()}
%% @doc Tries to read <code>Size</code> octets of data from
%% <code>Socket</code>. <code>Size</code> is only relevant if the socket is in
%% raw format.
-spec recv(gen_tcpd_socket(), non_neg_integer()) ->
	{ok, binary() | list()} | {error, atom()}.
recv({Mod, Socket}, Size) ->
	Mod:recv(Socket, Size).

%% @spec recv(Socket::socket(), Size::integer(), Timeout::integer()) -> Result
%% Result = {ok, Packet} | {error, Reason}
%% Reason = {error, timeout} | {error, posix()}
%% @doc Tries to read <code>Size</code> octets of data from
%% <code>Socket</code>. <code>Size</code> is only relevant if the socket is in
%% raw format. The recv request will return <code>{error, Timeout}</code> if
%% <code>Size</code> octets of data is not available within
%% <code>Timeout</code> milliseconds.
-spec recv(gen_tcpd_socket(), non_neg_integer(), timeout()) ->
	{ok, binary() | list()} | {error, atom()}.
recv({Mod, Socket}, Size, Timeout) ->
	Mod:recv(Socket, Size, Timeout).

%% @spec send(Socket::socket(), Packet) -> ok | {error, Reason}
%% Packet = [char()] | binary()
%% Reason = posix()
%% @doc Sends <code>Packet</code> on <code>Socket</code>.
-spec send(gen_tcpd_socket(), iolist() | binary()) -> ok | {error, atom()}.
send({Mod, Socket}, Packet) ->
	Mod:send(Socket, Packet).

%% @spec close(Socket::socket()) -> ok | {error, Reason}
%% Reason = posix()
%% @doc Closes <code>Socket</code>.
-spec close(gen_tcpd_socket()) -> ok | {error, atom()}.
close({Mod, Socket}) ->
	Mod:close(Socket).

%% @spec peername(Socket::socket()) -> {ok, {Address, Port}} | {error, Reason}
%% Address = ipaddress()
%% Port = integer()
%% Reason = posix()
%% @doc Returns the remote address and port of <code>Socket</code>.
-spec peername(gen_tcpd_socket()) -> {ok, {ip_address(), 1..65535}} | {error, atom()}.
peername({gen_tcp, Socket}) ->
	inet:peername(Socket);
peername({Mod, Socket}) ->
	Mod:peername(Socket).

%% @spec sockname(Socket::socket()) -> {ok, {Address, Port}} | {error, Reason}
%% Address = ipaddress()
%% Port = integer()
%% Reason = posix()
%% @doc Returns the local address and port of <code>Socket</code>.
-spec sockname(gen_tcpd_socket()) -> {ok, {ip_address(), 1..65535}} | {error, atom()}.
sockname({gen_tcp, Socket}) ->
	inet:sockname(Socket);
sockname({Mod, Socket}) ->
	Mod:sockname(Socket).

%% @spec type(Socket::socket()) -> tcp | ssl
%% @doc Returns the type of <code>Socket</code>. 
-spec type(gen_tcpd_socket()) -> tcp | ssl.
type({gen_tcp, _}) ->
	tcp;
type({ssl, _}) ->
	ssl;
type(A) ->
	exit({badarg, A}).

%% @spec controlling_process(Socket::socket(), Pid::pid()) ->
%%                                   ok | {error, Reason}
%% Reason = closed | not_owner | posix()
%% @doc Assigns a new controlling process <code>Pid</code> to
%% <code>Socket</code>.
-spec controlling_process(gen_tcpd_socket(), pid()) ->
	ok | {error, atom()}.
controlling_process({Mod, Socket}, Pid) ->
	Mod:controlling_process(Socket, Pid).

%% @spec setopts(Socket::socket(), Options) -> ok | {error, Reason}
%% Options = [{Option, Value} | Option]
%% Option = atom()
%% Value = term()
%% Reason = posix()
%% @doc Sets options for a socket.
%% See backend modules for more info.
-spec setopts(gen_tcpd_socket(), [{atom(), term()} | atom()]) ->
	ok | {error, atom()}.
setopts({gen_tcp, Socket}, Options) ->
	inet:setopts(Socket, Options);
setopts({Mod, Socket}, Options) ->
	Mod:setopts(Socket, Options).

%% @hidden
-spec init(any()) ->
	{ok, any()} | {ok, any(), timeout() | hibernate} | {stop, any()} | ignore.
init([Type, Mod, Args, Port, Options]) ->
	Acceptors = proplists:get_value(acceptors, Options, 1),
	Timeout = proplists:get_value(ssl_accept_timeout, Options, infinity),
	SocketOptions = proplists:get_value(socket_options, Options, []),
	case Mod:init(Args) of
		{ok, CState} ->
			case listen(module(Type), Port, SocketOptions) of
				{ok, Socket} ->
					start_acceptors(Acceptors, Mod, CState, Socket, Timeout),
					{ok, #state{
						callback = {Mod, CState},
						socket = Socket
					}};
				{error, Reason} ->
					{stop, Reason}
			end;
		{stop, Reason} ->
			Mod:terminate(Reason, undefined),
			{stop, Reason};
		Other ->
			{stop, {invalid_return_value, Other}}
	end.

%% @hidden
-spec handle_call(any(), {pid(), any()}, any()) ->
	{reply, any(), any()} | {reply, any(), any(), timeout() | hibernate} |
	{noreply, any()} | {noreply, any(), timeout() | hibernate} |
	{stop, any(), any(), any()} | {stop, any(), any()}.
handle_call(port, _, #state{socket = Socket} = State) ->
	{reply, sock_port(Socket), State};
handle_call(Request, _, State) ->
	{reply, {error, {bad_request, Request}}, State}.


%% @hidden
-spec handle_cast(any(), any()) ->
	{noreply, any()} | {noreply, any(), timeout() | hibernate} |
	{stop, any(), any()}.
handle_cast(stop, State) ->
	{stop, normal, State};
handle_cast(_, State) ->
	{noreply, State}.

%% @hidden
-spec handle_info(any(), any()) ->
	{noreply, any()} | {noreply, any(), timeout() | hibernate} |
	{stop, any(), any()}.
handle_info(Info, State) ->
	{CMod, CState} = State#state.callback,
	case CMod:handle_info(Info, CState) of
		{noreply, NewState} ->
			{noreply, NewState};
		{stop, Reason, NewState} ->
			{stop, Reason, NewState};
		Other ->
			exit({invalid_return_value, Other})
	end.

%% @hidden
-spec terminate(any(), any()) -> any().
terminate(Reason, #state{callback = {CMod, CState}} = State) ->
	close(State#state.socket),
	CMod:terminate(Reason, CState).

%% @hidden
-spec code_change(any(), any(), any()) ->
	{ok, any()}.
code_change(_, _, State) ->
	{ok, State}.

%% @private
start_acceptors(0, _, _, _, _) ->
	ok;
start_acceptors(Acceptors, Callback, CState, Socket, SSLTimeout) ->
	Args = [self(), Callback, CState, Socket, SSLTimeout],
	proc_lib:spawn(?MODULE, init_acceptor, Args),
	start_acceptors(Acceptors - 1, Callback, CState, Socket, SSLTimeout).

%% @hidden
-spec init_acceptor(pid(), atom(), term(), any(), timeout()) -> _.
init_acceptor(Parent, Callback, CState, Socket, SSLTimeout) ->
	try link(Parent)
		catch error:noproc -> exit(normal)
	end,
	put('$ancestors', tl(get('$ancestors'))),
	accept(Parent, Callback, CState, Socket, SSLTimeout).

accept(Parent, Callback, CState, Socket, SSLTimeout) ->
	case do_accept(Socket, SSLTimeout) of
		{ok, Client} ->
			Args = [Parent, Callback, CState, Socket, SSLTimeout],
			proc_lib:spawn(?MODULE, init_acceptor, Args),
			Callback:handle_connection(Client, CState);
		{error, {{ssl, ssl_accept}, timeout}} -> % SSL negotiation timed out
			accept(Parent, Callback, CState, Socket, SSLTimeout);
		{error, {_, closed}} ->
			unlink(Parent), % no need to send exit signals here
			exit(normal);
		Other ->
			erlang:error(Other)
	end.

listen(Mod, Port, Options) ->
	case Mod:listen(Port, Options) of
		{ok, Socket}    -> {ok, {Mod, Socket}};
		{error, Reason} -> {error, {{Mod, listen}, Reason}}
	end.

do_accept({ssl, Socket}, SSLTimeout) ->
	case ssl:transport_accept(Socket) of
		{ok, Client} ->
			case ssl:ssl_accept(Client, SSLTimeout) of
				ok ->
					{ok, {ssl, Client}};
				{error, Reason} ->
					{error, {{ssl, ssl_accept}, Reason}}
			end;
		{error, Reason} ->
			{error, {{ssl, transport_accept}, Reason}}
	end;
do_accept({Mod, Socket}, _) ->
	case Mod:accept(Socket) of
		{ok, Client}    -> {ok, {Mod, Client}};
		{error, Reason} -> {error, {{Mod, accept}, Reason}}
	end.

sock_port({gen_tcp, Socket}) ->
	element(2, inet:port(Socket));
sock_port({Mod, Socket}) ->
	element(2, Mod:port(Socket)).

check_options([{socket_options, List} | T]) when is_list(List) ->
	check_options(T);
check_options([{acceptors, N} | T]) when is_integer(N), N > 0 ->
	check_options(T);
check_options([{ssl_accept_timeout, infinity} | T]) ->
	check_options(T);
check_options([{ssl_accept_timeout, TO} | T]) when is_integer(TO), TO >= 0 ->
	check_options(T);
check_options([Option | _]) ->
	erlang:error({bad_option, Option});
check_options([]) ->
	ok.

module(tcp)  -> gen_tcp;
module(Type) -> Type.

%% @hidden
-spec behaviour_info(any()) -> [{atom(), non_neg_integer()}] | ok.
behaviour_info(callbacks) ->
	[{init, 1}, {handle_connection, 2}, {handle_info, 2}, {terminate, 2}];
behaviour_info(_) ->
	ok.
