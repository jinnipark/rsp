%%% -------------------------------------------------------------------
%%% Author  : Sungjin Park <jinni.park@gmail.com>
%%%
%%% Description : Rock-scissors-paper game.
%%%
%%% Created : Oct 3, 2013
%%% -------------------------------------------------------------------
-module(rsp_match_handler).
-author("Sungjin Park <jinni.park@gmail.com>").

-export([init/3, handle/2, terminate/3]).

-include("rsp.hrl").

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    do_match(cowboy_req:method(Req), State).

do_match({<<"POST">>, Req}, State) ->
    {ok, Params, Req1} = cowboy_req:body_qs(Req),
    Player = proplists:get_value(<<"player">>, Params),
    Move = proplists:get_value(<<"move">>, Params),
    {Match, Req2} = cowboy_req:binding(id, Req1),
    lager:debug("player ~p playing match ~p with move ~p", [Player, Match, Move]),
    {Code, Msg} = case rsp_match:play(Match, {Player, Move}) of
                      {wait, Pid, T} ->
                        Transport = cowboy_req:get(transport, Req2),
                        Socket = cowboy_req:get(socket, Req2),
                        Transport:setopts(Socket, [{active, once}]),
                        receive
                          {tcp_closed, Socket} ->
                            lager:debug("player ~p disconnected", [Player]),
                            {error, retry};
                          {ssl_closed, Socket} ->
                            lager:debug("player ~p disconnected", [Player]),
                            {error, retry};
                          {Pid, Result} ->
                            Result
                        after T ->
                          {error, timeout}
                        end;
                      Nowait ->
                        Nowait
                  end,
    lager:info("player ~p plays match ~p with move ~p resulting {~p, ~p}", [Player, Match, Move, Code, Msg]),
    {ok, Req3} = cowboy_req:reply(case {Code, Msg} of
                                      {ok, _} -> 200;
                                      {retry, _} -> 202;
                                      {error, illegal} -> 400;
                                      {error, closed} -> 400;
                                      {error, forbidden} -> 403;
                                      {error, not_found} -> 404;
                                      {error, timeout} -> 503;
                                      {error, _} -> 500
                                  end,
                                  [{<<"content-type">>, <<"application/json">>}],
                                  case catch jsx:encode(Msg) of
                                      {'EXIT', _} ->
                                          jsx:encode(erlang:list_to_binary(io_lib:format("~w", [Msg])));
                                      Json ->
                                          Json
                                  end, Req2),
    {ok, Req3, State};
do_match({<<"GET">>, Req}, State) ->
    {Id, Req1} = cowboy_req:binding(id, Req),
    {Rsc, Req2} = cowboy_req:binding(rsc, Req1),
    {Player, Req3} = cowboy_req:qs_val(<<"player">>, Req2),
    {Code, Msg} = case Rsc of
                      undefined -> rsp_match:get(Id);
                      <<"hint">> -> rsp_match:hint(Id, Player);
                      _ -> {error, bad_resource}
                  end,
    lager:info("get match ~p as ~p for player ~p resulting {~p, ~p}", [Id, Rsc, Player, Code, Msg]),
    {ok, Req4} = cowboy_req:reply(case {Code, Msg} of
                                      {ok, _} -> 200;
                                      {error, bad_resource} -> 400;
                                      {error, not_found} -> 404;
                                      {error, timeout} -> 503;
                                      {error, _} -> 500
                                  end,
                                  [{<<"content-type">>, <<"application/json">>}],
                                  case Msg of
                                      _=#rsp_match_tb{} ->
                                          Match = [{id, Msg#rsp_match_tb.id},
                                                   {player1, Msg#rsp_match_tb.player1_id},
                                                   {player2, Msg#rsp_match_tb.player2_id},
                                                   {status, case Msg#rsp_match_tb.ref of
                                                                Pid when erlang:is_pid(Pid) -> <<"open">>;
                                                                player1 -> <<"winner player1">>;
                                                                player2 -> <<"winner player2">>;
                                                                Other -> erlang:list_to_binary(io_lib:format("~w", [Other]))
                                                            end},
                                                   {event, Msg#rsp_match_tb.event_id}],
                                          jsx:encode(Match);
                                      _ ->
                                          case catch jsx:encode(Msg) of
                                              {'EXIT', _} ->
                                                  jsx:encode(erlang:list_to_binary(io_lib:format("~w", [Msg])));
                                              Json ->
                                                  Json
                                          end
                                  end, Req3),
    {ok, Req4, State};
do_match({Method, Req}, State) ->
    lager:warning("method not allowed ~p", [Method]),
    {ok, Req1} = cowboy_req:reply(405, [], <<>>, Req),
    {ok, Req1, State}.

terminate(Reason, _Req, _State) ->
    lager:debug("terminate ~p", [Reason]).

