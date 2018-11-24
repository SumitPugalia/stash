-module(stash_app_SUITE).

-export([all/0]).
-export([application_api/1]).

-spec all() -> [application_api].
all() -> [application_api].

-spec application_api(sa_ct:config()) -> ok.
application_api(_Config) ->
  false = is_app_running(),
  ok = stash_app:start(),
  true = is_app_running(),
  ok = stash_app:stop(),
  false = is_app_running(),
  ok.

%%====================================================================
%% Internal Functions
%%====================================================================
is_app_running() ->
  lists:member(stash, [App || {App, _, _} <- application:which_applications()]).