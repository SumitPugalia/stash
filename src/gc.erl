-module(gc).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% genserver callbacks
-export([init/1, handle_call/3, handle_info/2, handle_cast/2]).

-define(TABLE, stash).

%%====================================================================
%% API
%%====================================================================

-spec(start_link() -> {ok, pid()} | ignore | {error, term()}).
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%====================================================================
%% Gen Server Callback
%%====================================================================

init(_Args) ->
  Interval = application:get_env(stash, gc_interval, 15000),
  _ = erlang:send_after(Interval, ?MODULE, run_gc),
  {ok, #{gc_interval => Interval}}.

handle_info(run_gc, #{gc_interval := Interval} = State) ->
  ok = expire(),
  _ = erlang:send_after(Interval, ?MODULE, run_gc),
  {noreply, State};
handle_info(_Info, State) ->
  {noreply, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

expire() ->
  Now = time:timestamp(),
  _ = ets:select_delete(?TABLE, [
      {{'_', '_', '$3'}, [{'=<', '$3', Now}, {'>', '$3', 0}], [true]}
    ]),
  ok.
