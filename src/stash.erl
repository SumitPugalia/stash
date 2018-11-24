-module(stash).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([set/2, get/1, get/2, delete/1]).

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
  write(Key, Value).

-spec get(key()) -> value().
get(Key) ->
  get(Key, undefined).

-spec get(key(), value()) -> value().
get(Key, DefaultValue) ->
  Record = ets:lookup(?TABLE, Key),

  case Record of
    [] ->
      DefaultValue;
    [{Key, Value}] ->
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

write(Key, Value) ->
  true = ets:insert(?TABLE, {Key, Value}),
  ok.
