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

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    do_event(cowboy_req:method(Req), State).

do_event({<<"POST">>, Req}, State) ->
    {ok, Params, Req1} = cowboy_req:body_qs(Req),
    Player = proplists:get_value(<<"player">>, Params),
    {Event, Req2} = cowboy_req:binding(id, Req1),
    lager:debug("player ~p joining event ~p", [Player, Event]),
    {Code, Msg} = case rsp_event:join(Event, Player) of
                    {wait, Pid, T} ->
                      Transport = cowboy_req:get(transport, Req2),
                      Socket = cowboy_req:get(socket, Req2),
                      Transport:setopts(Socket, [{active, once}]),
                      receive
                        {Pid, Result} ->
                          catch Transport:setopts(Socket, [{active, none}]),
                          Result;
                        _ -> % All the other messages - including tcp_closed considered illegal
                          lager:debug("player ~p left", [Player]),
                          rsp_event:leave(Pid, Player),
                          catch Transport:setopts(Socket, [{active, none}]),
                          {error, retry}
                      after T ->
                          catch Transport:setopts(Socket, [{active, none}]),
                          {error, timeout}
                      end;
                    Result ->
                      Result
                  end,
    lager:info("player ~p joins event ~p resulting {~p, ~p}", [Player, Event, Code, Msg]),
    {ok, Req3} = cowboy_req:reply(case {Code, Msg} of
                                      {ok, _} -> 200;
                                      {error, not_found} -> 404;
                                      {error, timeout} -> 503;
                                      {error, _} -> 400
                                  end,
                                  [{<<"content-type">>, <<"application/json">>}],
                                  case catch jsx:encode(Msg) of
                                      {'EXIT', _} ->
                                          jsx:encode(erlang:list_to_binary(io_lib:format("~w", [Msg])));
                                      Json ->
                                          Json
                                  end, Req2),
    {ok, Req3, State};
do_event({<<"GET">>, Req}, State) ->
    {Val, Req1} = cowboy_req:binding(id, Req),
    Id = case Val of
             <<"all">> -> all;
             _ -> Val
         end,
    {Code, Msg} = rsp_event:get(Id),
    lager:info("get event ~p resulting {~p, ~p}", [Id, Code, Msg]),
    {ok, Req2} = cowboy_req:reply(case {Code, Msg} of
                                      {ok, _} -> 200;
                                      {error, not_found} -> 404;
                                      {error, timeout} -> 503;
                                      {error, _} -> 500
                                  end,
                                  [{<<"content-type">>, <<"application/json">>}],
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
                                          case catch jsx:encode(Msg) of
                                              {'EXIT', _} ->
                                                  jsx:encode(erlang:list_to_binary(io_lib:format("~w", [Msg])));
                                              Json ->
                                                  Json
                                          end
                                  end, Req1),
    {ok, Req2, State};
do_event({Method, Req}, State) ->
    lager:warning("method ~p not allowed", [Method]),
    {ok, Req1} = cowboy_req:reply(405, [], <<>>, Req),
    {ok, Req1, State}.

terminate(Reason, _Req, _State) ->
    lager:debug("terminate ~p", [Reason]).

