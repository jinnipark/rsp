%%% -------------------------------------------------------------------
%%% Author  : Sungjin Park <jinni.park@gmail.com>
%%%
%%% Description : Rock-scissors-paper event server.
%%%
%%% Created : Oct 3, 2013
%%% -------------------------------------------------------------------
-module(rsp_event).
-author("Sungjin Park <jinni.park@gmail.com>").
-behavior(gen_server).

-export([start/1, stop/1, join/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("props_to_record.hrl").
-include("rsp.hrl").

-record(?MODULE, {id,
                  name,
                  timeout=10000,
                  pool=[],
                  uuid}).

%%
%% Start an event
%%   [{id, binary()}, {name, binary()}]
%%
-spec start([{atom(), binary()}]) -> {ok, pid()} | {error, term()}.
start(Props) ->
    State = ?PROPS_TO_RECORD(Props ++ rsp:settings(?MODULE), ?MODULE),
    gen_server:start(?MODULE, State, []).

%%
%% Stop an event
%%   pid() | binary()
%%
-spec stop(pid() | binary()) -> ok | {error, term()}.
stop(Pid) when erlang:is_pid(Pid) ->
    gen_server:cast(Pid, stop);
stop(Id) when erlang:is_binary(Id) ->
    invoke(fun stop/1, [], Id);
stop(_) ->
    {error, not_supported}.

%%
%% Join an event
%%
-spec join(pid() | binary(), binary()) -> {ok, binary()} | {error, term()}.
join(Pid, Player) when erlang:is_pid(Pid) ->
    case gen_server:call(Pid, {join, Player}) of
        {wait, T} ->
            receive
                {Pid, Result} ->
                    Result
            after T ->
                {error, timeout}
            end;
        Result ->
            Result
    end;
join(Id, Player) when erlang:is_binary(Id) ->
    invoke(fun join/2, [Player], Id);
join(_, _) ->
    {error, not_supported}.

%%
%% gen_server callbacks
%%
init(State=#?MODULE{id=Id, name=Name}) ->
    lager:debug("init ~p", [Id]),
    Now = os:timestamp(),
    F = fun() ->
            {D, T} = calendar:now_to_universal_time(Now),
            mnesia:write(#rsp_event_tb{id=Id, name=Name, ref=self(),
                                       start_date=D, start_time=T})
        end,
    case catch mnesia:transaction(F) of
        {atomic, ok} ->
            erlang:process_flag(trap_exit, true),
            {ok, State#?MODULE{uuid=uuid:new(self(), os)}};
        Error ->
            {stop, Error, State}
    end.

handle_call({join, Player}, {Pid, _}, State=#?MODULE{id=Id, pool=Pool, timeout=T}) ->
    case lists:keyfind(Player, 1, Pool) of
        false ->
            case Pool of
                [] ->
                    erlang:link(Pid),
                    {reply, {wait, T}, State#?MODULE{pool=[{Player, Pid}]}};
                [{Player1, Pid1} | Rest] ->
                    MatchId = base32:encode(uuid:get_v4(), [lower, nopad]),
                    case catch rsp_match:start([{id, MatchId}, {player1, Player1}, {player2, Player}, {event, Id}]) of
                        {ok, _} ->
                            erlang:unlink(Pid1),
                            Pid1 ! {self(), {ok, MatchId}},
                            {reply, {ok, MatchId}, State#?MODULE{pool=Rest}};
                        Error ->
                            lager:warning("error ~p starting a match in event ~p", [Error, Id]),
                            {reply, {error, unknown}, State}
                    end
            end;
        _ ->
            {reply, {error, already_joined}, State}
    end;
handle_call(Req, _From, State) ->
    lager:warning("unknown call ~p", [Req]),
    {noreply, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(Msg, State) ->
    lager:warning("unknown cast ~p", [Msg]),
    {noreply, State}.

handle_info({'EXIT', Pid, Reason}, State=#?MODULE{id=Id, pool=Pool}) ->
    case lists:keytake(Pid, 2, Pool) of
        {value, {Player, Pid}, Rest} ->
            lager:debug("player ~p left ~p by ~p", [Player, Id, Reason]),
            {noreply, State#?MODULE{pool=Rest}};
        false ->
            lager:warning("unknown exit ~p from ~p", [Reason, Pid]),
            {noreply, State}
    end;
handle_info(Info, State) ->
    lager:warning("unknown info ~p", [Info]),
    {noreply, State}.

terminate(Reason, #?MODULE{id=Id}) ->
    Now = os:timestamp(),
    F = fun() ->
            case mnesia:wread({rsp_event_tb, Id}) of
                [Event] ->
                    {D, T} = calendar:now_to_universal_time(Now),
                    mnesia:write(Event#rsp_event_tb{ref=undefined,
                                                    end_date=D, end_time=T});
                _ ->
                    db_corrupt
            end
        end,
    case catch mnesia:transaction(F) of
        {atomic, ok} ->
            lager:debug("terminate ~p", [Reason]);
        Error ->
            lager:error("terminate ~p failed ~p", [Reason, Error])
    end.

code_change(_, State, _) ->
    {ok, State}.

%%
%% Local functions
%%
invoke(F, Args, Id) ->
    G = fun() ->
            mnesia:match_object(#rsp_event_tb{id=Id})
        end,
    case mnesia:transaction(G) of
        {atomic, [Event]} ->
            erlang:apply(F, [Event#rsp_event_tb.ref | Args]);
        {atomic, []} ->
            {error, not_found};
        {aborted, Reason} ->
            {error, Reason}
    end.
