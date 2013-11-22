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

init(Transport, Req, []) ->
    lager:debug("init ~p", [Transport]),
    {ok, Req, undefined}.

handle(Req, State) ->
    do_match(cowboy_req:method(Req), State).

do_match({<<"POST">>, Req}, State) ->
    {ok, Params, Req1} = cowboy_req:body_qs(Req),
    {Match, Req2} = cowboy_req:binding(id, Req1),
    lager:debug("join match ~p, params ~p", [Match, Params]),
    {Code, Msg} = rsp_match:play(Match, {proplists:get_value(<<"player">>, Params), proplists:get_value(<<"move">>, Params)}),
    {ok, Req3} = cowboy_req:reply(case {Code, Msg} of
                                      {ok, _} -> 200;
                                      {retry, _} -> 202;
                                      {error, illegal} -> 400;
                                      {error, match_closed} -> 400;
                                      {error, not_found} -> 404;
                                      {error, timeout} -> 503;
                                      {error, _} -> 500
                                  end, [], io_lib:format("~p~n", [Msg]), Req2),
    {ok, Req3, State};
do_match({<<"GET">>, Req}, State) ->
    {Id, Req1} = cowboy_req:binding(id, Req),
    lager:debug("get match ~p", [Id]),
    {Code, Msg} = rsp_match:get(Id),
    {ok, Req2} = cowboy_req:reply(case {Code, Msg} of
                                      {ok, _} -> 200;
                                      {error, not_found} -> 404;
                                      {error, timeout} -> 503;
                                      {error, _} -> 500
                                  end, [{<<"content-type">>, <<"application/json">>}],
                                  case Code of
                                      ok ->
                                          Match = [{player1, Msg#rsp_match_tb.player1_id},
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
                                          io_lib:format("~p~n", [Msg])
                                  end, Req1),
    {ok, Req2, State};
do_match({Method, Req}, State) ->
    lager:warning("method not allowed ~p", [Method]),
    {ok, Req1} = cowboy_req:reply(405, [], <<>>, Req),
    {ok, Req1, State}.

terminate(Reason, _Req, _State) ->
    lager:debug("terminate ~p", [Reason]).

