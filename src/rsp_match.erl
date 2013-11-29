%%% -------------------------------------------------------------------
%%% Author  : Sungjin Park <jinni.park@gmail.com>
%%%
%%% Description : Rock-scissors-paper match server.
%%%
%%% Created : Oct 2, 2013
%%% -------------------------------------------------------------------
-module(rsp_match).
-author("Sungjin Park <jinni.park@gmail.com>").
-behavior(gen_server).

-export([start/1, stop/1, play/2, get/1, get_opponent/2, hint/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("props_to_record.hrl").
-include("rsp.hrl").

-define (MAX_RECORD, 2).
-define (MAX_MOVES, 10).

-record(?MODULE, {timeout=60000,
				  timestamp,
				  id,
				  player1,
				  player2,
				  result,
                  event}).

%%
%% Start a match
%%   [{id, binary()}, {player1, binary()}, {player2, binary()}, {event, binary()}]
%%
-spec start([{atom(), term()}]) -> {ok, pid()} | {error, term()}.
start(Props) ->
	State = ?PROPS_TO_RECORD(Props ++ rsp:settings(?MODULE), ?MODULE),
	gen_server:start(?MODULE, State, []).

%%
%% Stop a match
%%   pid() | binary()
%%
-spec stop(pid() | binary()) -> ok | {error, term()}.
stop(Pid) when is_pid(Pid) ->
	gen_server:cast(Pid, stop);
stop(Id) when is_binary(Id) ->
	invoke(fun stop/1, [], Id);
stop(_) ->
	{error, not_supported}.

%%
%% Play a game - sync
%%   pid() | binary()
%%   {binary(), <<"rock">> | <<"scissors">> | <<"paper">>}
%%
-spec play(pid() | binary(), {binary(), binary()}) ->
          {ok, win | loss | abandoned} | {retry, draw} | {error, term()}.
play(_, {_, Move}) when (Move =/= <<"rock">>) and (Move =/= <<"scissors">>) and (Move =/= <<"paper">>) ->
    {error, illegal};
play(Pid, {Player, Move}) when is_pid(Pid) ->
    gen_server:call(Pid, {play, Player, erlang:binary_to_atom(Move, latin1)});
play(Id, {Player, Move}) when is_binary(Id) ->
	invoke(fun play/2, [{Player, Move}], Id);
play(_, _) ->
	{error, not_supported}.

get(Id) ->
	F = fun() ->
			mnesia:match_object(#rsp_match_tb{id=Id})
		end,
	case catch mnesia:transaction(F) of
		{atomic, [Match]} -> {ok, Match};
		{atomic, []} -> {error, not_found};
		Error -> {error, Error}
	end.

get_opponent(Pid, Player) when is_pid(Pid) ->
    gen_server:call(Pid, {opponent, Player});
get_opponent(Id, Player) when is_binary(Id) ->
	invoke(fun get_opponent/2, [Player], Id);
get_opponent(_, _) ->
	{error, not_supported}.

get_records(Player) ->
	F = fun() ->
			mnesia:select(rsp_match_tb,
						  [{#rsp_match_tb{ref='$1', % match head
						   				  player1_id='$2', player1_seq='$3',
								   		  player2_id='$4', player2_seq='$5'},
						   [{'and', {'or', {'==', '$2', Player}, {'==', '$4', Player}}, % guard
						   			{'or', {'==', '$1', player1}, {'==', '$1', player2}, {'==', '$1', draw}}}],
						   ['$$']}], % result
						  ?MAX_RECORD, read)
		end,
	case catch mnesia:transaction(F) of
		{atomic, {Records, _}} ->
			{ok, lists:map(fun([W, P1, M1, P2, M2]) ->
								  case {W, P1, P2} of
								  	  {player1, Player, _} ->
								  		  [{result, <<"win">>}, {moves, to_binary(M1)}];
								  	  {player1, _, Player} ->
								  	  	  [{result, <<"loss">>}, {moves, to_binary(M2)}];
								  	  {player2, Player, _} ->
								  		  [{result, <<"loss">>}, {moves, to_binary(M1)}];
								  	  {player2, _, Player} ->
								  	  	  [{result, <<"win">>}, {moves, to_binary(M2)}];
								  	  {draw, Player, _} ->
								  	  	  [{result, <<"draw">>}, {moves, to_binary(M1)}];
								  	  {draw, _, Player} ->
								  	  	  [{result, <<"draw">>}, {moves, to_binary(M2)}];
								  	  _ ->
								  	  	  []
								  end
						   end, Records)};
		{atomic, '$end_of_table'} ->
			{ok, []};
		Error ->
			{error, Error}
	end.

to_binary(Moves) ->
	lists:map(fun(Atom) -> erlang:list_to_binary(io_lib:format("~w", [Atom])) end, Moves).

hint(Id, Player) when is_binary(Id) ->
	case get_opponent(Id, Player) of
		{ok, Opponent} ->
			get_records(Opponent);
		Error ->
			Error
	end.

%%
%% gen_server callbacks
%%
init(State=#?MODULE{timeout=To, id=Id, player1=P1, player2=P2}) ->
	lager:debug("init ~p for ~p vs ~p", [Id, P1, P2]),
	{A, B, C} = os:timestamp(),
	F = fun() ->
			mnesia:write(#rsp_match_tb{timestamp={-A, -B, -C}, % for LIFO
									   id=Id, ref=self(),
									   player1_id=P1, player2_id=P2,
									   player1_seq=[], player2_seq=[],
                                       event_id=State#?MODULE.event})
		end,
	case catch mnesia:transaction(F) of
		{atomic, ok} ->
			{ok, State#?MODULE{timestamp={A, B, C}, player1={P1, [], undefined}, player2={P2, [], undefined}}, To};
		Error ->
			{stop, Error, State}
	end.

handle_call({play, Player, Move}, {Pid, _}, State=#?MODULE{timeout=To}) ->
    Now = os:timestamp(),
    case {Player, State#?MODULE.player1, State#?MODULE.player2} of
        {Player, {Player, L1, undefined}, {Player2, L2, Pid2}} ->
            L = [Move | L1],
            case Pid2 of
                undefined ->
                    lager:debug("player1 ~p first move ~p", [Player, Move]),
                    {reply, {wait, self(), To*2}, State#?MODULE{timestamp=Now, player1={Player, L, Pid}}, To};
                _ ->
                    lager:debug("player1 ~p last move ~p", [Player, Move]),
                    P2 = {Player2, L2, undefined},
                    case compare(Move, lists:nth(1, L2)) of
                        win ->
                            lager:debug("player1 win"),
                            Pid2 ! {self(), {ok, loss}},
                            {stop, normal, {ok, win}, State#?MODULE{result=player1, player1={Player, L, undefined}, player2=P2}};
                        draw ->
                            lager:debug("draw"),
                            case erlang:length(L) of
                            	Len when Len >= ?MAX_MOVES ->
        		                    Pid2 ! {self(), {ok, draw}},
                                    {stop, normal, {ok, draw}, State#?MODULE{result=draw, player1={Player, L, undefined}, player2=P2}};
                            	_ ->
		                            Pid2 ! {self(), {retry, draw}},
                            		{reply, {retry, draw}, State#?MODULE{timestamp=Now, player1={Player, L, undefined}, player2=P2}, To}
                            end;
                        loss ->
                            lager:debug("player1 loss"),
                            Pid2 ! {self(), {ok, win}},
                            {stop, normal, {ok, loss}, State#?MODULE{result=player2, player1={Player, L, undefined}, player2=P2}};
                        Error ->
                            lager:debug("error ~p", [Error]),
                            {reply, {error, Error}, State, timeout(State)}
                    end
            end;
        {Player, {Player1, L1, Pid1}, {Player, L2, undefined}} ->
            L = [Move | L2],
            case Pid1 of
                undefined ->
                    lager:debug("player2 ~p first move ~p", [Player, Move]),
                    {reply, {wait, self(), To*2}, State#?MODULE{timestamp=Now, player2={Player, L, Pid}}, To};
                _ ->
                    lager:debug("player2 ~p last move ~p", [Player, Move]),
                    P1 = {Player1, L1, undefined},
                    case compare(Move, lists:nth(1, L1)) of
                        win ->
                            lager:debug("player2 win"),
                            Pid1 ! {self(), {ok, loss}},
                            {stop, normal, {ok, win}, State#?MODULE{result=player2, player1=P1, player2={Player, L, undefined}}};
                        draw ->
                            case erlang:length(L) of
                            	Len when Len >= ?MAX_MOVES ->
        		                    Pid1 ! {self(), {ok, draw}},
                                    {stop, normal, {ok, draw}, State#?MODULE{result=draw, player1=P1, player2={Player, L, undefined}}};
                            	_ ->
		                            Pid1 ! {self(), {retry, draw}},
                            		{reply, {retry, draw}, State#?MODULE{timestamp=Now, player1=P1, player2={Player, L, undefined}}, To}
                            end;
                        loss ->
                            lager:debug("player2 loss"),
                            Pid1 ! {self(), {ok, win}},
                            {stop, normal, {ok, loss}, State#?MODULE{result=player1, player1=P1, player2={Player, L, undefined}}};
                        Error ->
                            lager:debug("error ~p", [Error]),
                            {reply, {error, Error}, State, timeout(State)}
                    end
            end;
        _ ->
            % something illegal
            {reply, {error, forbidden}, State, timeout(State)}
    end;
handle_call({opponent, Player}, _From, State=#?MODULE{player1={Player, _, _}, player2={Opponent, _, _}}) ->
	{reply, {ok, Opponent}, State, timeout(State)};
handle_call({opponent, Player}, _From, State=#?MODULE{player1={Opponent, _, _}, player2={Player, _, _}}) ->
	{reply, {ok, Opponent}, State, timeout(State)};
handle_call({opponent, _Player}, _From, State) ->
	{reply, {error, forbidden}, State, timeout(State)};
handle_call(Req, _From, State) ->
	lager:warning("unknown call ~p", [Req]),
	{noreply, State, timeout(State)}.

handle_cast(stop, State) ->
	{stop, normal, State};
handle_cast(Msg, State) ->
	lager:warning("unknown cast ~p", [Msg]),
	{noreply, State, timeout(State)}.

handle_info(timeout, State=#?MODULE{player1={P1, L1, Pid1}, player2={P2, L2, Pid2}}) ->
	% notify to waiting player
    case Pid1 of
        undefined -> ok;
        _ -> Pid1 ! {self(), {ok, abandoned}}
    end,
    case Pid2 of
        undefined -> ok;
        _ -> Pid2 ! {self(), {ok, abandoned}}
    end,
    {stop, normal, State#?MODULE{result=abandoned, player1={P1, L1, undefined}, player2={P2, L2, undefined}}};
handle_info(Info, State) ->
	lager:warning("unknown info ~p", [Info]),
	{noreply, State, timeout(State)}.

terminate(Reason, State=#?MODULE{id=Id, player1={_,L1,_}, player2={_,L2,_}}) ->
	F = fun() ->
			case mnesia:match_object(#rsp_match_tb{id=Id}) of
				[Match] ->
					mnesia:write(Match#rsp_match_tb{ref=State#?MODULE.result,
													player1_seq=L1, player2_seq=L2});
				_ ->
					db_corrupt
			end
		end,
	case catch mnesia:transaction(F) of
		{atomic, ok} ->
			lager:debug("terminate ~p", [Reason]);
		Error ->
			lager:error("terminate ~p failed ~p", [Reason,Error])
	end.

code_change(_, State, _) ->
	{ok, State}.

%%
%% Local functions
%%
invoke(F, Args, Id) ->
	G = fun() ->
			mnesia:match_object(#rsp_match_tb{id=Id})
		end,
	case mnesia:transaction(G) of
		{atomic, [Match]} ->
			case Match#rsp_match_tb.ref of
				Pid when erlang:is_pid(Pid) ->
					case rsp:is_alive(Pid) of
						true ->
							erlang:apply(F, [Pid | Args]);
						_ -> % Dead match, abandon.
							{atomic, ok} = mnesia:transaction(fun() ->
																  mnesia:write(Match#rsp_match_tb{ref=abandoned})
															  end),
							{error, abandoned}
					end;
				abandoned ->
					{error, abandoned};
				_ ->
					{error, closed}
			end;
		{atomic, []} ->
			{error, not_found};
		{aborted, Reason} ->
			{error, Reason}
	end.

compare(rock, scissors) ->
	win;
compare(scissors, paper) ->
	win;
compare(paper, rock) ->
	win;
compare(rock, paper) ->
	loss;
compare(scissors, rock) ->
	loss;
compare(paper, scissors) ->
	loss;
compare(_, _) ->
    draw.

timeout(#?MODULE{timeout=To, timestamp=Ts}) ->
	erlang:max(0, To - timer:now_diff(os:timestamp(), Ts) div 1000).
