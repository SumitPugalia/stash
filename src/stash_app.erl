%%%-------------------------------------------------------------------
%% @doc stash public API
%% @end
%%%-------------------------------------------------------------------

-module(stash_app).

-behaviour(application).

%% API
-export([start/0, stop/0]).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

-spec start() -> ok.
start() ->
  {ok, _} = application:ensure_all_started(stash),
  ok.

-spec stop() -> ok.
stop() ->
  application:stop(stash).

%%====================================================================
%% Application Callback
%%====================================================================
start(_StartType, _StartArgs) ->
  Nodes = application:get_env(stash, nodes, []),
  ok = lists:foreach(fun net_adm:ping/1, Nodes),
  stash_sup:start_link().

stop(_State) ->
  ok.
