-module(stash_app_SUITE).

-export([all/0]).
-export([application_api/1]).

-spec all() -> [application_api].
all() -> [application_api].

-spec application_api(stash_ct:config()) -> ok.
application_api(Config) ->
  false = is_app_running(),
  _Config1 = stash_ct:start_application(Config),
  true = is_app_running(),
  ok = stash_ct:stop_application(),
  false = is_app_running(),
  ok.

%%====================================================================
%% Internal Functions
%%====================================================================
is_app_running() ->
  lists:member(stash, [App || {App, _, _} <- application:which_applications()]).