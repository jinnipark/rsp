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

start(_StartType, _StartArgs) ->
    rsp_sup:start_link().

stop(_State) ->
    ok.
