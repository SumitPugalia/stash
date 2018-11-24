-module(stash_ct).

-type config() :: proplists:proplist().
-export_type([config/0]).

-export([start_application/1, stop_application/0]).

-spec start_application(config()) -> config().
start_application(Config) ->
  ok = stash_app:start(),
  Config.

-spec stop_application() -> ok.
stop_application() ->
  stash_app:stop().
