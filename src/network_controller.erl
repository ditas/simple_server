-module(network_controller).

-behaviour(gen_server).

-include("common.hrl").

%% API
-export([
    start_link/0,
    update_match/2
]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    port,
    socket,
    matches = []
}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

update_match(MatchId, Status) ->
    gen_server:cast(?SERVER, {update, MatchId, Status}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->

    _ = process_flag(trap_exit, true),

    {ok, Port} = application:get_env(?APPLICATION, incom_port),
    {ok, Socket} = gen_udp:open(Port, [binary, {active, once}]),
    {ok, #state{port = Port, socket = Socket}}.

-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                  State :: #state{}) ->
                     {reply, Reply :: term(), NewState :: #state{}} |
                     {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
                     {noreply, NewState :: #state{}} |
                     {noreply, NewState :: #state{}, timeout() | hibernate} |
                     {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
                     {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast({update, MatchId, Status}, #state{matches = Matches} = State) ->
    State1 = case lists:keytake(MatchId, 1, Matches) of
        {MatchId, {_, Pid, _}, Matches1} ->
            State#state{matches = [{MatchId, Pid, Status}|Matches1]};
        _ ->
            State
    end,
    {noreply, State1};
handle_cast(_Request, State) ->
    {noreply, State}.

-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info({udp, Socket, RemoteIp, RemotePort, Data}, State) when State#state.socket =:= Socket ->

    io:format("------NC------Incoming Data ~p~n", [Data]),

    State1 = case binary:split(Data, <<" ">>, [global]) of
        [<<"init">>, TimeStamp, MatchId, PlayerId] ->
            handle_match_q(MatchId, PlayerId, TimeStamp, RemoteIp, RemotePort, State);
        _ -> State
    end,

    self() ! reactivate,

    {noreply, State1};
handle_info({'EXIT', Reason}, State) ->

    io:format("------NC------Match crashed with message ~p~n", [Reason]),

    {noreply, State};
handle_info(reactivate, #state{port = Port, socket = Socket0} = State) ->

    io:format("------NC------Reactivating socket~n"),

    ok = gen_udp:close(Socket0),
    {ok, Socket} = gen_udp:open(Port, [binary, {active, once}]),
    {noreply, State#state{port = Port, socket = Socket}};
handle_info(Info, State) ->

    io:format("------NC------Unknown message ~p~n", [Info]),

    {noreply, State}.

-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
                State :: #state{}) -> term()).
terminate(_Reason, _State) ->
    ok.

-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
                  Extra :: term()) ->
                     {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
handle_match_q(MatchId, PlayerId, TimeStamp, RemoteIp, RemotePort, #state{matches = Matches} = State) ->
    case lists:keyfind(MatchId, 1, Matches) of
        {MatchId, Pid, Status} when Status =:= standby ->

            io:format("------NC-------MATCH is waiting~n"),

            _ = gen_server:cast(Pid, {add_player, PlayerId, {RemoteIp, RemotePort}}),

            State;
        {MatchId, _Pid, _Status} ->

            io:format("------NC-------MATCH is already started~n"),

            State;
        _ ->
            {ok, Pid} = match_serv:start_link(MatchId, PlayerId, TimeStamp, {RemoteIp, RemotePort}),

            io:format("------NC-------MATCH is starting~n"),

            State#state{matches = [{MatchId, Pid, standby}|Matches]}
    end.

    % {state,<<"test_match">>,2,<<"1607992581">>,undefined,[<<"test_player">>]}
