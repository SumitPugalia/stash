-module(time).

-export([timestamp/0]).

-spec timestamp() -> non_neg_integer().
timestamp() ->
  erlang:system_time(millisecond).