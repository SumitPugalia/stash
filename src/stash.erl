-module(stash).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([set/2, set/3, get/1, get/2, delete/1]).

%% genserver callbacks
-export([init/1, handle_call/3, handle_info/2, handle_cast/2]).

-define(TABLE, ?MODULE).
%%====================================================================
%% Types
%%====================================================================

-type key() :: any().
-type value() :: any().

%%====================================================================
%% API
%%====================================================================

-spec(start_link() -> {ok, pid()} | ignore | {error, term()}).
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec set(key(), value()) -> ok.
set(Key, Value) ->
  set(Key, Value, 0).

-spec set(key(), value(), non_neg_integer()) -> ok.
set(Key, Value, Ttl) ->
  Expiry = expire_at(Ttl),
  write(Key, Value, Expiry).

-spec get(key()) -> value().
get(Key) ->
  get(Key, undefined).

-spec get(key(), value()) -> value().
get(Key, DefaultValue) ->
  Record = ets:lookup(?TABLE, Key),

  case may_be_expired(Record) of
    true ->
      DefaultValue;
    false ->
      [{Key, Value, Expiry}] = Record,
      Value
  end.

-spec delete(key()) -> ok.
delete(Key) ->
  true = ets:delete(?TABLE, Key),
  ok.

%%====================================================================
%% Gen Server Callback
%%====================================================================

init(_Args) ->
  ?TABLE = create_table(),
  {ok, #{}}.

handle_info(_Info, State) ->
  {noreply, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

create_table() ->
  case ets:info(?TABLE, name) of
    undefined ->
      ets:new(?TABLE, [named_table, set, public, {read_concurrency, true}]);
    ?TABLE ->
      ?TABLE
  end.

write(Key, Value, Expiry) ->
  true = ets:insert(?TABLE, {Key, Value, Expiry}),
  ok.

may_be_expired([]) ->
  true;
may_be_expired([{_Key, _Value, 0}]) ->
  false;
may_be_expired([{_Key, _Value, Expiry}]) ->
  Expiry < timestamp().

expire_at(0) -> 0;
expire_at(Ttl) -> timestamp() + (Ttl * 1000).

timestamp() -> erlang:system_time(millisecond).