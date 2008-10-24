%%% ----------------------------------------------------------------------------
%%% Copyright 2008
%%% Martin Carlson, martin@erlang-consulting.com
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
%%% @author Martin Carlson <martin@erlang-consulting.com>
%%% @author Oscar Hellström <oscar@hellstrom.st> [http://oscar.hellstrom.st]
%%% @version {@version}, {@date}, {@time}
%%% @doc
%%% 
%%% @end
%%% ----------------------------------------------------------------------------
-module(gen_tcpd).
-behaviour(gen_server).

-export([start_link/5]).
-export([
		send/2,
		recv/3,
		close/1,
		controlling_process/2,
		peername/1,
		sockname/1,
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

start_link(Callback, CallbackArgs, tcp, Port, Options) ->
	gen_server:start_link(?MODULE, 
		[gen_tcp, {Callback, CallbackArgs}, Port, Options], []);
start_link(Callback, CallbackArgs, ssl, Port, Options) ->
	gen_server:start_link(?MODULE, 
		[ssl, {Callback, CallbackArgs}, Port, Options], []).

recv({Mod, Socket}, Size, Timeout) ->
	Mod:recv(Socket, Size, Timeout).

send({Mod, Socket}, Packet) ->
	Mod:send(Socket, Packet).

close({Mod, Socket}) ->
	Mod:close(Socket).

peername({gen_tcp, Socket}) ->
	inet:peername(Socket);
peername({Mod, Socket}) ->
	Mod:peername(Socket).

sockname({gen_tcp, Socket}) ->
	inet:sockname(Socket);
sockname({Mod, Socket}) ->
	Mod:sockname(Socket).

type({gen_tcp, _}) ->
	tcp;
type({ssl, _}) ->
	ssl;
type(_) ->
	exit(badarg).

controlling_process({Mod, Socket}, Pid) ->
	Mod:controlling_process(Socket, Pid).

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

handle_call({new_connection, Socket}, _From, State) ->
	{CMod, CState} = State#state.callback,
	case CMod:handle_connection(Socket, CState) of
		{noreply, CState0} ->
			{reply, noreply, State#state{callback = {CMod, CState0}}};
		{stop, Reason, CState0} ->
			{stop, Reason, stop, State#state{callback = {CMod, CState0}}}
	end.

handle_cast(_, State) ->
	{noreply, State}.

handle_info({'EXIT', Acceptor, Reason}, #state{acceptor = Acceptor}) ->
	exit(Reason); % to preserve original behaviuor if someone traps exits
handle_info(Info, #state{callback = {CMod, CState}} = State) ->
	case CMod:handle_info(Info, CState) of
		{noreply, CState0} ->
			{noreply, State#state{callback = {CMod, CState0}}};
		Other ->
			exit({invalid_return_value, Other})
	end.

terminate(Reason, #state{callback = {CMod, CState}} = State) ->
	close(State#state.socket),
	CMod:terminate(Reason, CState).

code_change(_, _, State) ->
	{ok, State}.

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
		{ok, Socket} -> {ok, {Mod, Socket}};
		Error        -> Error
	end.

accept({Mod, Socket}) ->
	case Mod:accept(Socket) of
		{ok, Client} -> {ok, {Mod, Client}};
		Error        -> Error
	end.

%% @hidden
behaviour_info(callbacks) ->
	[{init, 1}, {handle_connection, 2}, {handle_info, 2}, {terminate, 2}];
behaviour_info(_) ->
	ok.
