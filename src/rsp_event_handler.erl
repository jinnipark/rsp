%%% -------------------------------------------------------------------
%%% Author  : Sungjin Park <jinni.park@gmail.com>
%%%
%%% Description : Rock-scissors-paper event api handler.
%%%
%%% Created : Oct 3, 2013
%%% -------------------------------------------------------------------
-module(rsp_event_handler).
-author("Sungjin Park <jinni.park@gmail.com>").

-export([init/3, handle/2, terminate/3]).

-include("rsp.hrl").

init(Transport, Req, []) ->
    lager:debug("init ~p", [Transport]),
    {ok, Req, undefined}.

handle(Req, State) ->
    do_event(cowboy_req:method(Req), State).

do_event({<<"POST">>, Req}, State) ->
    {ok, Params, Req1} = cowboy_req:body_qs(Req),
    {Event, Req2} = cowboy_req:binding(id, Req1),
    lager:debug("event ~p, params ~p", [Event, Params]),
    {Code, Msg} = rsp_event:join(Event, proplists:get_value(<<"player">>, Params)),
    {ok, Req3} = cowboy_req:reply(case {Code, Msg} of
                                      {ok, _} -> 200;
                                      {error, not_found} -> 404;
                                      {error, timeout} -> 503;
                                      {error, _} -> 400
                                  end, [], io_lib:format("~s~n", [Msg]), Req2),
    {ok, Req3, State};
do_event({<<"GET">>, Req}, State) ->
    {Val, Req1} = cowboy_req:binding(id, Req),
    Id = case Val of
             <<"all">> -> all;
             _ -> Val
         end,
    lager:debug("get event ~p", [Id]),
    {Code, Msg} = rsp_event:get(Id),
    {ok, Req2} = cowboy_req:reply(case {Code, Msg} of
                                      {ok, _} -> 200;
                                      {error, not_found} -> 404;
                                      {error, timeout} -> 503;
                                      {error, _} -> 500
                                  end, [{<<"content-type">>, <<"application/json">>}],
                                  case Code of
                                      ok ->
                                          jsx:encode(case Msg of
                                                         Events when erlang:is_list(Events) ->
                                                             lists:map(fun(Event) ->
                                                                           [{id, Event#rsp_event_tb.id},
                                                                            {name, case Event#rsp_event_tb.name of
                                                                                       undefined -> <<>>;
                                                                                       Name -> Name
                                                                                   end},
                                                                            {status, case Event#rsp_event_tb.ref of
                                                                                         Pid when erlang:is_pid(Pid) -> <<"open">>;
                                                                                         _ -> <<"closed">>
                                                                                     end}]
                                                                       end, Events);
                                                         Event ->
                                                             [{id, Event#rsp_event_tb.id},
                                                              {name, case Event#rsp_event_tb.name of
                                                                         undefined -> <<>>;
                                                                         Name -> Name
                                                                     end},
                                                              {status, case Event#rsp_event_tb.ref of
                                                                           Pid when erlang:is_pid(Pid) -> <<"open">>;
                                                                           _ -> <<"closed">>
                                                                       end}]
                                                     end);
                                      _ ->
                                          io_lib:format("~p~n", [Msg])
                                  end, Req1),
    {ok, Req2, State};
do_event({Method, Req}, State) ->
    lager:warning("method not allowed ~p", [Method]),
    {ok, Req1} = cowboy_req:reply(405, [], <<>>, Req),
    {ok, Req1, State}.

terminate(Reason, _Req, _State) ->
    lager:debug("terminate ~p", [Reason]).

