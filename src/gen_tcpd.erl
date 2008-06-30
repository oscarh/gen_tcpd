-module(gen_tcpd).
-behaviour(gen_server).

-export([start_link/5]).
-export([send/2, recv/3, close/1]).
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).
-export([acceptor_loop/3]).
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

init([Type, Callback, Port, Options]) ->
	process_flag(trap_exit, true),
	{ok, Socket} = listen(Type, Port, Options),
	{ok, Acceptor} = acceptor(Socket, Callback),
	{ok, #state{callback = Callback, socket = Socket, acceptor = Acceptor}}.

handle_call(_, _From, State) ->
	{reply, ok, State}.

handle_cast({spawn_acceptor, Pid}, State) when Pid == State#state.acceptor ->
	{ok, Acceptor} = acceptor(State#state.socket, State#state.callback),
	{noreply, State#state{acceptor = Acceptor}};
handle_cast(_, State) ->
	{noreply, State}.

handle_info({'EXIT', _, normal}, State) ->
	{noreply, State};
handle_info({'EXIT', Pid, _}, State) when Pid == State#state.acceptor ->
	{ok, Acceptor} = acceptor(State#state.socket, State#state.callback),
	{noreply, State#state{acceptor = Acceptor}};
handle_info(_, State) ->
	{noreply, State}.

terminate(_, State) ->
	close(State#state.socket),
	ok.
code_change(_, _, State) ->
	{ok, State}.

acceptor(Socket, Callback) ->
	Parent = self(),
	{ok, spawn_link(fun() -> 
		case catch ?MODULE:acceptor_loop(Parent, Socket, Callback) of
			{'EXIT', Reason} -> exit(Reason); %% Prevent SASL error reports...
			Result           -> Result
		end
	end)}.

acceptor_loop(Parent, Socket, {Callback, CallbackArgs}) ->
	case Callback:init(CallbackArgs) of
		{ok, CallbackState} ->
			{ok, Client} = accept(Socket),
			gen_server:cast(Parent, {spawn_acceptor, self()}),
			unlink(Parent),
			% FIXME Hey, no one is supervising this process!
			Callback:handle_connection(Client, CallbackState);
		Other ->
			exit({bad_return, Other})
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
	[{init, 1}, {handle_connection, 2}];
behaviour_info(_) ->
	ok.
