-module(stash).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([set/2, set/3, write/1, get/1, get/2, delete/1, all/0]).

%% genserver callbacks
-export([init/1, handle_call/3, handle_info/2, handle_cast/2]).

%% test
-export([count/0, clear/0]).

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
  broadcast(stash, write, [{Key, Value, Expiry}]).

-spec write({key(), value(), non_neg_integer()}) -> ok.
write(Record) ->
  try
    true = ets:insert(?TABLE, Record)
  catch
    error:badarg ->
      ok = gen_server:cast(?MODULE, {write, Record})
  end,
  ok.

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
      [{Key, Value, _Expiry}] = Record,
      Value
  end.

-spec delete(key()) -> ok.
delete(Key) ->
  true = ets:delete(?TABLE, Key),
  ok.

-spec all() -> [tuple()].
all() ->
  ets:tab2list(?TABLE).

-spec count() -> non_neg_integer().
count() ->
  length(all()).

-spec clear() -> ok.
clear() ->
  true = ets:delete_all_objects(?TABLE),
  ok.

%%====================================================================
%% Gen Server Callback
%%====================================================================

init(_Args) ->
  ?TABLE = create_table(),
  ok = replicate_data(),
  {ok, #{}}.

handle_cast({write, Record}, State) ->
  true = ets:insert(?TABLE, Record),
  {noreply, State};
handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
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

broadcast(Mod, Fun, Args) ->
  Nodes = [node() | nodes()],
  _ = rpc:multicall(Nodes, Mod, Fun, Args),
  ok.

replicate_data() ->
  Objects = get_objects(nodes()),
  true = ets:insert(?TABLE, Objects),
  ok.

get_objects([]) ->
  [];
get_objects([Node | Nodes]) ->
  case rpc:call(Node, stash, all, []) of
    [] ->
      get_objects(Nodes);
    Records ->
      Records
  end.

may_be_expired([]) ->
  true;
may_be_expired([{_Key, _Value, 0}]) ->
  false;
may_be_expired([{_Key, _Value, Expiry}]) ->
  Expiry < time:timestamp().

expire_at(0) -> 0;
expire_at(Ttl) -> time:timestamp() + (Ttl * 1000).
