%%% -------------------------------------------------------------------
%%% Author  : Sungjin Park <jinni.park@gmail.com>
%%%
%%% Description : Rock-scissors-paper game application callback.
%%%
%%% Created : Sep 27, 2013
%%% -------------------------------------------------------------------
-module(rsp_app).
-author("Sungjin Park <jinni.park@gmail.com>").
-behaviour(application).

-export([start/2, stop/1]).

-record(?MODULE, {acceptors=100}).

-include("props_to_record.hrl").

start(_StartType, _StartArgs) ->
    start_listener(),
    rsp_sup:start_link().

stop(_State) ->
    ok.

start_listener() ->
    Dispatch = cowboy_router:compile([
        {'_', [ % host match
            {"/match/:id/[:rsc]", rsp_match_handler, []},
            {"/event/:id", rsp_event_handler, []}
        ]}
    ]),
    Config = ?PROPS_TO_RECORD(rsp:settings(?MODULE), ?MODULE),
    case string:to_integer(os:getenv("RSP_PORT")) of
        {error, _} ->
            ok;
        {Port, _} ->
            {ok, _} = cowboy:start_http(http, Config#?MODULE.acceptors,
                                        [{port, Port}], [{env, [{dispatch, Dispatch}]}]),
            lager:notice("http listener started on port ~p", [Port])
    end.

