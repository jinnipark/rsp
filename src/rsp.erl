%%% -------------------------------------------------------------------
%%% Author  : Sungjin Park <jinni.park@gmail.com>
%%%
%%% Description : Rock-scissors-paper game.
%%%
%%% Created : Sep 27, 2013
%%% -------------------------------------------------------------------
-module(rsp).
-author("Sungjin Park <jinni.park@gmail.com>").

-export([start/0, stop/0, state/0, settings/1]).

-define(MNESIA_TIMEOUT, 10000).

-include("rsp.hrl").
-include_lib("stdlib/include/qlc.hrl").

-spec start() -> ok | {error, term()}.
start() ->
	% create application dependency store
	ets:new(?MODULE, [named_table, public]),
	ensure_started(kernel),
	ensure_started(stdlib),
	ensure_started(lager),
	ensure_started(mnesia),
    ensure_started(cowboy),
	Res = application:start(?MODULE),
	% let the store persist
	ets:setopts(?MODULE, [{heir, whereis(rsp_sup), ?MODULE}]),
	Res.

-spec stop() -> ok | {error, term()}.
stop() ->
	Deps = lists:sort(fun([_, T1], [_, T2]) ->
						  timer:now_diff(T1, T2) > 0
					  end,
					  ets:match(?MODULE, {'$1', '$2'})),
	ets:delete(?MODULE),
	application:stop(?MODULE),
	% stop all the dependencies
	ensure_stopped(Deps),
	halt().

-spec state() -> {ok, term()} | {error, term()}.
state() ->
	case lists:keyfind(?MODULE, 1, application:which_applications()) of
		false ->
			{error, "not running"};
		{Name, Desc, Version} ->
			{ok, [{name, Name}, {description, Desc}, {version, Version}]}
	end.

-spec settings(module()) -> [{atom(), term()}].
settings(Module) ->
	case application:get_env(?MODULE, Module) of
		{ok, Props} -> Props;
		_ -> []
	end.

%%
%% Local functions
%%
ensure_started(App) ->
	prestart(App),
	case application:start(App) of
		ok ->
			ets:insert(?MODULE, {App, os:timestamp()}),
			poststart(App);
		{error, {already_started, _}} ->
			ok;
		{error, {not_started, Dep}} ->
			ensure_started(Dep),
			ensure_started(App)
	end.

ensure_stopped([]) ->
	ok;
ensure_stopped([[App, _] | T]) ->
	prestop(App),
	application:stop(App),
	poststop(App),
	ensure_stopped(T).

prestart(mnesia) ->
	case list_to_atom(os:getenv("RSP_MASTER")) of
		undefined ->
			% schema must be created in standalone mode
			mnesia:create_schema([node()]);
		Master ->
			% join a cluster and clear stale replica
			pong = net_adm:ping(Master),
			lists:foreach(fun(N) ->
							  rpc:call(N, mnesia, del_table_copy, [schema, node()])
						  end,
						  nodes())
	end;
prestart(_) ->
	ok.

poststart(mnesia) ->
	case list_to_atom(os:getenv("RSP_MASTER")) of
		undefined ->
			% create tables
			mnesia:create_table(rsp_match_tb, [{attributes, record_info(fields, rsp_match_tb)},
											   {disc_copies, [node()]}, {type, ordered_set},
											   {index, [start_date, start_time]}]),
            mnesia:create_table(rsp_event_tb, [{attributes, record_info(fields, rsp_event_tb)},
                                               {disc_copies, [node()]}, {type, set},
                                               {index, []}]),
			ok = mnesia:wait_for_tables([rsp_match_tb, rsp_event_tb], ?MNESIA_TIMEOUT);
			% Q = qlc:q([Event || Event <- mnesia:table(rsp_event_tb),
			% 					erlang:is_pid(Event#rsp_event_tb.ref)]),
			% T = fun() ->
			% 		Events = qlc:e(Q),
			% 		lists:foreach(fun(Event) ->
			% 						  rsp_event:start([{id, Event#rsp_event_tb.id},
			% 						  				   {name, Event#rsp_event_tb.name},
			% 						  				   {start_date, Event#rsp_event_tb.start_date},
			% 						  				   {start_time, Event#rsp_event_tb.start_time}])
			% 					  end, Events)
			% 	end,
			% ok = mnesia:async_dirty(T);
		Master ->
			% create fresh replicas
			{ok, _} = rpc:call(Master, mnesia, change_config, [extra_db_nodes, [node()]]),
			mnesia:change_table_copy_type(schema, node(), disc_copies),
			{atomic, ok} = mnesia:add_table_copy(rsp_match_tb, node(), disc_copies),
            {atomic, ok} = mnesia:add_table_copy(rsp_event_tb, node(), disc_copies)
	end;
poststart(_) ->
	ok.

prestop(_) ->
	ok.

poststop(mnesia) ->
	% clear replica before leaving the cluster
	lists:foreach(fun(N) ->
                      case node() of
                          N -> ok;
					      _ -> rpc:call(N, mnesia, del_table_copy, [schema, node()])
                      end
				  end,
				  nodes());
poststop(_) ->
	ok.
