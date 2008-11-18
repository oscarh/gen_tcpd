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
%%% After {@link start_link/5} has been called this function
%%% is called by the new to initialise a state. If the the initialisation is
%%% successful the function should return <code>{ok, State}</code> where
%%% <code>State</code> is the state which will be passed to the client in
%%% in the next callback. <code>Args</code> is the <code>Args</code> passed
%%% to {@link start_link/5}.
%%%
%%% <pre>
%%% Module:handle_connection(Socket, State) -> Result
%%%     Types Socket = {@link socket()}
%%%           State = term()
%%%           Result = {noreply, NewState} | {stop, Reason, NewState}
%%% </pre>
%%% When a TCP connection is received from a client, this function is
%%% called. No other TCP connection can be accepted before this function has
%%% returned, but they can be backlogged. If the socket should be controlled
%%% by anoother process, give away control by {@link controlling_process/2}.
%%%
%%% If <code>Result</code> is <code>{noreply, NewState}</code> the server
%%% will wait for another connection. <code>NewState</code> is the possibly
%%% updated internal state. If <code>Result</code> is <code>{stop, Reason,
%%% NewState}</code> the server will exit with <code>Reason</code>, after
%%% calling <code>Module:terminate(Reason, NewState)</code>.
%%%
%%% <pre>
%%% Module:handle_info(Info, State) -> Result
%%%     Types Info = term()
%%%           State = term()
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
%%% This function will be called if any of the other callbacks return
%%% <code>{stop, Reason}</code>.
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
		type/1
	]).
-export([
		init/1,
		handle_call/3,
		handle_cast/2,
		handle_info/2,
		code_change/3,
		terminate/2
	]).
-export([safe_acceptor_loop/2]).
-export([behaviour_info/1]).

-record(state, {callback, acceptor, socket}).

%% @spec start_link(Callback, CallbackArg, Type, Port, Options) -> {ok, Pid}
%% Callback = atom()
%% CallbackArg = term()
%% Type = tcp | ssl
%% Port = integer()
%% Options = [Opt]
%% Pid = pid()
%% @doc Starts a gen_tcpd process and links to it.
%% <code>Callback</code> is the module that implements the specific parts of
%% the behaivour. <code>CallbackArg</code> is the arguments that will be
%% passed to <code>Module:init/1</code>. <code>Type</code> defines what type
%% of socket the server should use. <code>Port</code> is the TCP port that
%% the server will listen on. <code>Options</code> will
%% be passed to the backend module for setting up the socket.
%% This function should normally be called from a supervisor.
start_link(Callback, CallbackArg, tcp, Port, Options) ->
	gen_server:start_link(?MODULE, 
		[gen_tcp, {Callback, CallbackArg}, Port, Options], []);
start_link(Callback, CallbackArg, ssl, Port, Options) ->
	gen_server:start_link(?MODULE, 
		[ssl, {Callback, CallbackArg}, Port, Options], []).

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
port(Ref) ->
	gen_server:call(Ref, port).

%% @spec recv(Socket::socket(), Size::integer()) -> Result
%% Result = {ok, Packet} | {error, Reason}
%% Reason = {error, timeout} | {error, posix()}
%% @doc Tries to read <code>Size</code> octets of data from
%% <code>Socket</code>. <code>Size</code> is only relevant if the socket is in
%% raw format.
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
recv({Mod, Socket}, Size, Timeout) ->
	Mod:recv(Socket, Size, Timeout).

%% @spec send(Socket::socket(), Packet) -> ok | {error, Reason}
%% Packet = [char()] | binary()
%% Reason = posix()
%% @doc Sends <code>Packet</code> on <code>Socket</code>.
send({Mod, Socket}, Packet) ->
	Mod:send(Socket, Packet).

%% @spec close(Socket::socket()) -> ok | {error, Reason}
%% Reason = posix()
%% @doc Closes <code>Socket</code>.
close({Mod, Socket}) ->
	Mod:close(Socket).

%% @spec peername(Socket::socket()) -> {ok, {Address, Port}} | {error, Reason}
%% Address = ipaddress()
%% Port = integer()
%% Reason = posix()
%% @doc Returns the remote address and port of <code>Socket</code>.
peername({gen_tcp, Socket}) ->
	inet:peername(Socket);
peername({Mod, Socket}) ->
	Mod:peername(Socket).

%% @spec sockname(Socket::socket()) -> {ok, {Address, Port}} | {error, Reason}
%% Address = ipaddress()
%% Port = integer()
%% Reason = posix()
%% @doc Returns the local address and port of <code>Socket</code>.
sockname({gen_tcp, Socket}) ->
	inet:sockname(Socket);
sockname({Mod, Socket}) ->
	Mod:sockname(Socket).

%% @spec type(Socket::socket()) -> tcp | ssl
%% @doc Returns the type of <code>Socket</code>. 
type({gen_tcp, _}) ->
	tcp;
type({ssl, _}) ->
	ssl;
type(_) ->
	exit(badarg).

%% @spec controlling_process(Socket::socket(), Pid::pid()) ->
%%                                   ok | {error, Reason}
%% Reason = closed | not_owner | posix()
%% @doc Assigns a new controlling process <code>Pid</code> to
%% <code>Socket</code>.
controlling_process({Mod, Socket}, Pid) ->
	Mod:controlling_process(Socket, Pid).

%% @spec setopts(Socket::socket(), Options) -> ok | {error, Reason}
%% Reason = posix()
%% @doc Sets options for a socket.
%% See backend modules for more info.
setopts({gen_tcp, Socket}, Options) ->
	inet:setopts(Socket, Options);
setopts({Mod, Socket}, Options) ->
	Mod:setopts(Socket, Options).

%% @hidden
init([Type, {Mod, Args}, Port, Options]) ->
	case Mod:init(Args) of
		{ok, CState} ->
			case listen(Type, Port, Options) of
				{ok, Socket} ->
					{ok, Acceptor} = acceptor(Socket),
					{ok, #state{
						callback = {Mod, CState}, 
						socket = Socket,
						acceptor = Acceptor
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
handle_call({new_connection, Socket}, _From, State) ->
	{CMod, CState} = State#state.callback,
	case CMod:handle_connection(Socket, CState) of
		{noreply, CState0} ->
			{reply, noreply, State#state{callback = {CMod, CState0}}};
		{stop, Reason, CState0} ->
			{stop, Reason, stop, State#state{callback = {CMod, CState0}}}
	end;
handle_call(port, _, #state{socket = Socket} = State) ->
	{reply, sock_port(Socket), State}.

%% @hidden
handle_cast(_, State) ->
	{noreply, State}.

%% @hidden
handle_info({'EXIT', Acceptor, Reason}, #state{acceptor = Acceptor}) ->
	exit(Reason); % to preserve original behaviuor if someone traps exits
handle_info(Info, #state{callback = {CMod, CState}} = State) ->
	case CMod:handle_info(Info, CState) of
		{noreply, CState0} ->
			{noreply, State#state{callback = {CMod, CState0}}};
		Other ->
			exit({invalid_return_value, Other})
	end.

%% @hidden
terminate(Reason, #state{callback = {CMod, CState}} = State) ->
	close(State#state.socket),
	CMod:terminate(Reason, CState).

%% @hidden
code_change(_, _, State) ->
	{ok, State}.

%% @private
acceptor(Socket) ->
	Parent = self(),
	{ok, spawn_link(?MODULE, safe_acceptor_loop, [Parent, Socket])}.

%% @hidden
safe_acceptor_loop(Parent, Socket) ->
	case catch acceptor_loop(Parent, Socket) of
		%% Prevent SASL error reports...
		{'EXIT', Reason} -> exit({accept_error, Reason});
		_                -> ok % The acceptor loop should never return
	end.

acceptor_loop(Parent, Socket) ->
	{ok, Client} = accept(Socket),
	ok = controlling_process(Client, Parent),
	case gen_server:call(Parent, {new_connection, Client}) of
		noreply ->
			acceptor_loop(Parent, Socket);
		stop ->
			exit(normal)
	end.

listen(Mod, Port, Options) ->
	case Mod:listen(Port, Options) of
		{ok, Socket}    -> {ok, {Mod, Socket}};
		{error, Reason} -> {error, {Mod, listen}, Reason}
	end.

accept({Mod, Socket}) ->
	case Mod:accept(Socket) of
		{ok, Client}    -> {ok, {Mod, Client}};
		{error, Reason} -> {error, {Mod, accept}, Reason}
	end.

sock_port({gen_tcp, Socket}) ->
	inet:port(Socket);
sock_port({Mod, Socket}) ->
	Mod:port(Socket).

%% @hidden
behaviour_info(callbacks) ->
	[{init, 1}, {handle_connection, 2}, {handle_info, 2}, {terminate, 2}];
behaviour_info(_) ->
	ok.
