-module(stash_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([
  all/0,
  init_per_suite/1,
  end_per_suite/1
]).

-export([curd/1]).

-spec all() -> [atom()].
all() -> [curd].

-spec init_per_suite(stash_ct:config()) -> stash_ct:config().
init_per_suite(Config) ->
  Config1 = stash_ct:start_application(Config),
  Config1.

-spec end_per_suite(stash_ct:config()) -> ok.
end_per_suite(_) ->
  stash_ct:stop_application().

-spec curd(stash_ct:config()) -> ok.
curd(_Config) ->
  undefined = stash:get(a),
  nil = stash:get(a, nil),

  ok = stash:set(a, 10),
  10 = stash:get(a),

  ok = stash:delete(a),
  undefined = stash:get(a),

  ok.
