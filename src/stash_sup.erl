%%%-------------------------------------------------------------------
%% @doc stash top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(stash_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  Children = [#{
    id => stash,
    start => {stash, start_link, []},
    restart => permanent
  }],
  {ok, {{one_for_one, 5, 10}, Children}}.

%%====================================================================
%% Internal functions
%%====================================================================
