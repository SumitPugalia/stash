-module(coverage_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([
  all/0,
  init_per_suite/1,
  end_per_suite/1
]).

-export([stash/1, gc/1]).

-spec all() -> [atom()].
all() -> [stash, gc].

-spec init_per_suite(stash_ct:config()) -> stash_ct:config().
init_per_suite(Config) ->
  Config1 = stash_ct:start_application(Config),
  Config1.

-spec end_per_suite(stash_ct:config()) -> ok.
end_per_suite(_) ->
  stash_ct:stop_application().

-spec stash(stash_ct:config()) -> ok.
stash(_Config) ->
  _ = gen_server:cast(stash, {write, {key, value}}),
  _ = gen_server:cast(stash, any),
  _ = gen_server:call(stash, any),
  _ = stash ! any,
  ok.

-spec gc(stash_ct:config()) -> ok.
gc(_Config) ->
  _ = gen_server:cast(gc, any),
  _ = gen_server:call(gc, any),
  _ = gc ! any,
  ok.