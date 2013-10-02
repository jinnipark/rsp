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

-spec start() -> ok | {error, term()}.
start() ->
	% create application dependency store
	ets:new(?MODULE, [named_table, public]),
	ensure_started(kernel),
	ensure_started(stdlib),
	ensure_started(lager),
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

prestart(_) ->
	ok.

poststart(_) ->
	ok.

prestop(_) ->
	ok.

poststop(_) ->
	ok.
