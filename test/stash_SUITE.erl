-module(stash_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([
  all/0,
  init_per_suite/1,
  end_per_suite/1,
  init_per_testcase/2,
  end_per_testcase/2
]).

-export([curd/1, ttl/1, gc/1]).

-spec all() -> [atom()].
all() -> [curd, ttl, gc].

-spec init_per_suite(stash_ct:config()) -> stash_ct:config().
init_per_suite(Config) ->
  Config1 = stash_ct:start_application(Config),
  Config1.

-spec end_per_suite(stash_ct:config()) -> ok.
end_per_suite(_) ->
  stash_ct:stop_application().

-spec init_per_testcase(any(), stash_ct:config()) -> stash_ct:config().
init_per_testcase(_Case, Config) ->
  Config.

-spec end_per_testcase(any(), stash_ct:config()) -> ok.
end_per_testcase(_Case, _Config) ->
  stash:clear().

-spec curd(stash_ct:config()) -> ok.
curd(_Config) ->
  undefined = stash:get(name),
  nil = stash:get(name, nil),

  ok = stash:set(name, "Sumit"),
  "Sumit" = stash:get(name),

  ok = stash:delete(name),
  undefined = stash:get(name),

  ok.

-spec ttl(stash_ct:config()) -> ok.
ttl(_Config) ->
  undefined = stash:get(name),

  % never expired
  ok = stash:set(name, "Sumit", 0),
  "Sumit" = stash:get(name),

  % expired after 1 second
  ok = stash:set(city, "Dubai", 1),
  "Dubai" = stash:get(city),

  % wait for a second
  timer:sleep(1100),
  undefined = stash:get(city),
  "Sumit" = stash:get(name),

  ok.

-spec gc(stash_ct:config()) -> ok.
gc(_Config) ->
  undefined = stash:get(name),

  % never expired
  ok = stash:set(name, "Sumit", 0),
  "Sumit" = stash:get(name),

  % expired after 1 second
  ok = stash:set(city, "Dubai", 1),
  "Dubai" = stash:get(city),
  true = validate_count(2),

  % wait for a second
  timer:sleep(1000),
  undefined = stash:get(city),
  "Sumit" = stash:get(name),
  true = validate_count(2),

  timer:sleep(1000),
  undefined = stash:get(city),
  "Sumit" = stash:get(name),

  % expired entries removed
  true = validate_count(1),

  ok.

% private
validate_count(ExpectedCount) ->
  true =
    ktn_task:wait_for_success(fun() ->
      ExpectedCount = stash:count(),
      true
    end, 1000, 5).
