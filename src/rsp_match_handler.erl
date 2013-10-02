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
    lager:debug("params ~p", [Params]),
    {ok, Req2} = cowboy_req:reply(200, [], <<>>, Req1),
    {ok, Req2, State};
do_match({Method, Req}, State) ->
    lager:warning("method not allowed ~p", [Method]),
    {ok, Req1} = cowboy_req:reply(405, [], <<>>, Req),
    {ok, Req1, State}.

terminate(Reason, _Req, _State) ->
    lager:debug("terminate ~p", [Reason]).

