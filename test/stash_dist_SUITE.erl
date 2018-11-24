-module(stash_dist_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([
  all/0,
  init_per_suite/1,
  end_per_suite/1,
  init_per_testcase/2,
  end_per_testcase/2
]).

-export([curd/1]).

-define(SLAVES, [node1, node2]).

-spec all() -> [atom()].
all() -> [curd].

-spec init_per_suite(stash_ct:config()) -> stash_ct:config().
init_per_suite(Config) ->
  Config1 = stash_ct:start_application(Config),
  Config1.

-spec end_per_suite(stash_ct:config()) -> ok.
end_per_suite(_) ->
  stash_ct:stop_application().

-spec init_per_testcase(any(), stash_ct:config()) -> stash_ct:config().
init_per_testcase(_Case, Config) ->
  ok = stash:set(name, "Sumit"),
  [HNode1, HNode2] = HostNodes = start_slaves(?SLAVES),
  pong = rpc:call(HNode1, net_adm, ping, [HNode2]),
  [{hostnodes, HostNodes} | Config].

-spec end_per_testcase(any(), stash_ct:config()) -> ok.
end_per_testcase(_Case, _Config) ->
  ok = stash:clear(),
  _ = stop_slaves(?SLAVES),
  ok.

-spec curd(stash_ct:config()) -> ok.
curd(Config) ->
  [HNode1, HNode2] = ?config(hostnodes, Config),

  ok = stash:set(city, "Dubai"),

  "Dubai" = stash:get(city),
  "Dubai" = cmd(HNode1, stash, get, [city]),
  "Dubai" = cmd(HNode2, stash, get, [city]),

  ok = cmd(HNode1, stash, set, [profile, "Erlang"]),

  "Erlang" = stash:get(profile),
  "Erlang" = cmd(HNode1, stash, get, [profile]),
  "Erlang" = cmd(HNode2, stash, get, [profile]),

  ok.

% private

start_slaves(Slaves) ->
  start_slaves(Slaves, []).

start_slaves([], HostNodes) ->
  lists:usort(HostNodes);
start_slaves([Node | Nodes], HostNodes) ->
  ErlFlags = "-pa ../../lib/stash/ebin -config ../../../../test/test_dist.config ",

  {ok, HostNode} =
    ct_slave:start(Node, [
      {kill_if_fail, true},
      {monitor_master, true},
      {boot_timeout, 5},
      {init_timeout, 3},
      {startup_timeout, 5},
      {startup_functions, [{application, ensure_all_started, [stash]}]},
      {erl_flags, ErlFlags}
    ]),
  start_slaves(Nodes, [HostNode | HostNodes]).

stop_slaves(Slaves) ->
  stop_slaves(Slaves, []).

stop_slaves([], Acc) ->
  lists:usort(Acc);
stop_slaves([Node | Nodes], Acc) ->
  {ok, _Name} = ct_slave:stop(Node),
  pang = net_adm:ping(Node),
  stop_slaves(Nodes, [Node | Acc]).

cmd(Node, Mod, Fun, Args) ->
  rpc:call(Node, Mod, Fun, Args).
