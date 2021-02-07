-module(match_serv).

-behaviour(gen_server).

-include("common.hrl").

%% API
-export([start_link/4]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).
-define(PLAYER_START_TIMEOUT_MS, 1000).

-record(state, {
    match_id,
    req_players_num,
    init_time,
    start_time,
    players = []
}).

%%%===================================================================
%%% API
%%%===================================================================
start_link(MatchId, PlayerId, TimeStamp, ConnectionData) ->
    gen_server:start_link(?MODULE, [MatchId, PlayerId, TimeStamp, ConnectionData], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([MatchId, PlayerId, TimeStamp, ConnectionData]) ->
    _ = timer:send_after(?PLAYER_START_TIMEOUT_MS, {start_player, PlayerId, ConnectionData}),
    ReqPlayersNum = application:get_env(?APPLICATION, req_players_num, ?DEFAULT_PLAYERS_NUM),
    {ok, #state{match_id = MatchId, req_players_num = ReqPlayersNum, init_time = TimeStamp, players = [{PlayerId, undefined}]}}.

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
handle_cast({add_player, PlayerId, ConnectionData}, #state{match_id = MatchId, req_players_num = ReqPlayersNum, players = Players} = State) ->
    _ = timer:send_after(?PLAYER_START_TIMEOUT_MS, {start_player, PlayerId, ConnectionData}),
    State1 = case length(Players) + 1 == ReqPlayersNum of
        true ->
            network_controller:update_match(MatchId, started),
            State#state{start_time = erlang:system_time(seconds)};
        false ->
            State
    end,
    {noreply, State1#state{players = [{PlayerId, undefined}|Players]}};
handle_cast({update, PlayerId, TimeStamp, PlayerState}, #state{players = Players} = State) ->
    case lists:keytake(PlayerId, 1, Players) of
        {_, _, Players1} -> send_update(Players1, PlayerId, TimeStamp, PlayerState);
        _ -> ignore
    end,
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info({start_player, PlayerId, ConnectionData}, #state{players = Players} = State) ->
    {ok, Pid} = player_serv:start_link(PlayerId, ConnectionData),

    io:format("------MS------ PLAYER is starting ~p~n", [PlayerId]),

    Players1 = lists:keydelete(PlayerId, 1, Players),
    Players2 = [{PlayerId, Pid}|Players1],
    {noreply, State#state{players = Players2}};
handle_info(_Info, State) ->

    io:format("------MS------Unknown message ~p~n", [_Info]),

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
send_update(Players, PlayerId, TimeStamp, PlayerState) ->
    lists:foreach(fun({_, Pid}) ->
        gen_server:cast(Pid, {update, PlayerId, TimeStamp, PlayerState})
    end, Players).
