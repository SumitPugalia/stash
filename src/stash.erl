-module(stash).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% genserver callbacks
-export([init/1, handle_call/3, handle_info/2, handle_cast/2]).

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
