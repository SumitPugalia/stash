-module(time).

-export([timestamp/0]).

-spec timestamp() -> integer().
timestamp() ->
  erlang:system_time(millisecond).