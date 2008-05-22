-module(gen_ssld).
-export([start_link/4]).
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).
-export([acceptor_loop/3]).

-record(state, {callback, acceptor, socket}).

start_link(Callback, CallbackArgs, Port, Options) ->
	gen_server:start_link(?MODULE, [{Callback, CallbackArgs}, Port, Options], []).

init([Callback, Port, Options]) ->
	process_flag(trap_exit, true),
	{ok, Socket} = ssl:listen(Port, Options),
	{ok, Acceptor} = acceptor(Socket, Callback),
	{ok, #state{callback = Callback, socket = Socket, acceptor =  Acceptor}}.

handle_call(_, _From, State) ->
	{reply, ok, State}.

handle_cast({spaw_acceptor, Pid}, State) when Pid == State#state.acceptor ->
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
	ssl:close(State#state.socket),
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
	{ok, CallbackState} = Callback:init(CallbackArgs),
	{ok, Client} = ssl:accept(Socket),
	gen_server:cast(Parent, {spaw_acceptor, self()}),
	Callback:handle_request(Client, CallbackState).
