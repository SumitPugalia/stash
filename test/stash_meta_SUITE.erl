-module(stash_meta_SUITE).

-include_lib("mixer/include/mixer.hrl").
-mixin([ktn_meta_SUITE]).

-export([init_per_suite/1, end_per_suite/1]).

-spec init_per_suite(stash_ct:config()) -> stash_ct:config().
init_per_suite(Config) -> [{application, stash} | Config].

-spec end_per_suite(stash_ct:config()) -> ok.
end_per_suite(_) -> ok.