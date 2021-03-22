-module(player_serv).

-behaviour(gen_server).

-include("common.hrl").

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    parent_pid,
    player_id,
    remote_ip,
    remote_port,
    socket
}).

-define(TIMEOUT, 500).

%%%===================================================================
%%% API
%%%===================================================================
start_link(PlayerId, {RemoteIp, RemotePort}) ->
    gen_server:start_link(?MODULE, [PlayerId, RemoteIp, RemotePort, self()], []). %% self() is for match_serv

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([PlayerId, RemoteIp, RemotePort, ParentPid]) ->
    gen_server:cast(self(), {response, RemoteIp, RemotePort}),
    {ok, #state{player_id = PlayerId, parent_pid = ParentPid}}.

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
handle_cast({response, RemoteIp, RemotePort}, State) ->
    {ok, S} = gen_udp:open(0, [binary, {active, true}]),
    Res = gen_udp:send(S, {RemoteIp, RemotePort}, "server_init_response"),

    io:format("------PS------response Res ~p~n", [Res]),

    {noreply, State#state{socket = S, remote_ip = RemoteIp, remote_port = RemotePort}};
handle_cast({update, PlayerId, _TimeStamp, {PlayerX,
        PlayerY,
        PlayerWidth,
        PlayerHeight,
        PlayerBaseSpeed,
        PlayerMaxSpeed,
        PlayerAction,
        PlayerAngle,
        PlayerTime,
        PlayerFixX,
        PlayerFixY,
        PlayerThrowAngleTimeMultiplier,
        PlayerStatusL,
        PlayerStatusT,
        PlayerStatusR,
        PlayerStatusB,
        PlayerDirection,
        PlayerIsJump,

        PlayerShoot,
        PlayerProjAngle,
        PlayerProjStartCoordsX,
        PlayerProjStartCoordsY,

        PlayerPlatformX,
        PlayerPlatformY,
        PlayerPlatformW,
        PlayerPlatformH}}, #state{remote_ip = RemoteIp, remote_port = RemotePort, socket = S} = State) ->
    gen_udp:send(S, {RemoteIp, RemotePort}, "update "
        ++ binary_to_list(PlayerId)
        ++ " "
        ++ binary_to_list(PlayerX)
        ++ " "
        ++ binary_to_list(PlayerY)
        ++ " "
        ++ binary_to_list(PlayerWidth)
        ++ " "
        ++ binary_to_list(PlayerHeight)
        ++ " "
        ++ binary_to_list(PlayerBaseSpeed)
        ++ " "
        ++ binary_to_list(PlayerMaxSpeed)
        ++ " "
        ++ binary_to_list(PlayerAction)
        ++ " "
        ++ binary_to_list(PlayerAngle)
        ++ " "
        ++ binary_to_list(PlayerTime)
        ++ " "
        ++ binary_to_list(PlayerFixX)
        ++ " "
        ++ binary_to_list(PlayerFixY)
        ++ " "
        ++ binary_to_list(PlayerThrowAngleTimeMultiplier)
        ++ " "
        ++ binary_to_list(PlayerStatusL)
        ++ " "
        ++ binary_to_list(PlayerStatusT)
        ++ " "
        ++ binary_to_list(PlayerStatusR)
        ++ " "
        ++ binary_to_list(PlayerStatusB)
        ++ " "
        ++ binary_to_list(PlayerDirection)
        ++ " "
        ++ binary_to_list(PlayerIsJump)

        ++ " "
        ++ binary_to_list(PlayerShoot)
        ++ " "
        ++ binary_to_list(PlayerProjAngle)
        ++ " "
        ++ binary_to_list(PlayerProjStartCoordsX)
        ++ " "
        ++ binary_to_list(PlayerProjStartCoordsY)

        ++ " "
        ++ binary_to_list(PlayerPlatformX)
        ++ " "
        ++ binary_to_list(PlayerPlatformY)
        ++ " "
        ++ binary_to_list(PlayerPlatformW)
        ++ " "
        ++ binary_to_list(PlayerPlatformH)),
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info({udp, Socket, _RemoteIp, _RemotePort, Data}, State) when State#state.socket =:= Socket ->

    io:format("------PS------Incoming Data ~p~n", [Data]),

    State1 = case binary:split(Data, <<" ">>, [global]) of
        [<<"move">>, TimeStamp,
                PlayerX,
                PlayerY,
                PlayerWidth,
                PlayerHeight,
                PlayerBaseSpeed,
                PlayerMaxSpeed,
                PlayerAction,
                PlayerAngle,
                PlayerTime,
                PlayerFixX,
                PlayerFixY,
                PlayerThrowAngleTimeMultiplier,
                PlayerStatusL,
                PlayerStatusT,
                PlayerStatusR,
                PlayerStatusB,
                PlayerDirection,
                PlayerIsJump,

                PlayerShoot,
                PlayerProjAngle,
                PlayerProjStartCoordsX,
                PlayerProjStartCoordsY,

                PlayerPlatformX,
                PlayerPlatformY,
                PlayerPlatformW,
                PlayerPlatformH
            ] ->
            handle_move(TimeStamp, {PlayerX,
                PlayerY,
                PlayerWidth,
                PlayerHeight,
                PlayerBaseSpeed,
                PlayerMaxSpeed,
                PlayerAction,
                PlayerAngle,
                PlayerTime,
                PlayerFixX,
                PlayerFixY,
                PlayerThrowAngleTimeMultiplier,
                PlayerStatusL,
                PlayerStatusT,
                PlayerStatusR,
                PlayerStatusB,
                PlayerDirection,
                PlayerIsJump,

                PlayerShoot,
                PlayerProjAngle,
                PlayerProjStartCoordsX,
                PlayerProjStartCoordsY,

                PlayerPlatformX,
                PlayerPlatformY,
                PlayerPlatformW,
                PlayerPlatformH}, State);
        _ -> State
    end,
    {noreply, State};
handle_info(Info, State) ->

    io:format("------PS------Unknown message ~p~n", [Info]),

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
handle_move(TimeStamp, PlayerState, #state{player_id = PlayerId, parent_pid = ParentPid} = State) ->
    gen_server:cast(ParentPid, {update, PlayerId, TimeStamp, PlayerState}),
    State.
