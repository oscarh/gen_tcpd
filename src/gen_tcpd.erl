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
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).
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

peername({ssl, Socket}) ->
	ssl:peername(Socket);
peername({_, Socket}) ->
	inet:peername(Socket).

sockname({ssl, Socket}) ->
	ssl:sockname(Socket);
sockname({_, Socket}) ->
	inet:sockname(Socket).

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
			{ok, Socket} = listen(Type, Port, Options),
			{ok, Acceptor} = acceptor(Socket),
			{ok, #state{
				callback = {Mod, CState}, 
				socket = Socket,
				acceptor = Acceptor
			}};
		Other ->
			Other
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

handle_info(Info, #state{callback = {CMod, CState}} = State) ->
	{noreply, CState0} = CMod:handle_info(Info, CState),
	{noreply, State#state{callback = {CMod, CState0}}}.

terminate(Reason, #state{callback = {CMod, CState}} = State) ->
	close(State#state.socket),
	CMod:terminate(Reason, CState).

code_change(_, _, State) ->
	{ok, State}.

acceptor(Socket) ->
	Parent = self(),
	{ok, spawn_link(?MODULE, safe_acceptor_loop, [Parent, Socket])}.

safe_acceptor_loop(Parent, Socket) ->
	case catch acceptor_loop(Parent, Socket) of
		%% Prevent SASL error reports...
		{'EXIT', Reason} -> exit(Reason);
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

behaviour_info(callbacks) ->
	[{init, 1}, {handle_connection, 2}, {handle_info, 2}, {terminate, 2}];
behaviour_info(_) ->
	ok.
