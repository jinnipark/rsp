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

init(Transport, Req, []) ->
    lager:debug("init ~p", [Transport]),
    {ok, Req, undefined}.

handle(Req, State) ->
    do_match(cowboy_req:method(Req), State).

do_match({<<"POST">>, Req}, State) ->
    {ok, Params, Req1} = cowboy_req:body_qs(Req),
    {Match, Req2} = cowboy_req:binding(id, Req1),
    lager:debug("match ~p, params ~p", [Match, Params]),
    {Code, Msg} = rsp_match:play(Match, {proplists:get_value(<<"player">>, Params), proplists:get_value(<<"move">>, Params)}),
    {ok, Req3} = cowboy_req:reply(case {Code, Msg} of
                                      {ok, _} -> 200;
                                      {retry, _} -> 202;
                                      {error, timeout} -> 503;
                                      {error, _} -> 400
                                  end, [], io_lib:format("~p", [Msg]), Req2),
    {ok, Req3, State};
do_match({Method, Req}, State) ->
    lager:warning("method not allowed ~p", [Method]),
    {ok, Req1} = cowboy_req:reply(405, [], <<>>, Req),
    {ok, Req1, State}.

terminate(Reason, _Req, _State) ->
    lager:debug("terminate ~p", [Reason]).

