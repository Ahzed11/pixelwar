-module(distributed_SUITE).
-behaviour(ct_suite).
-export([all/0, groups/0]).
-compile(export_all).
-define(SERVER_NAME, "matrix").

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

groups() ->
    [{upgrade_downgrade, [sequence], [before_upgrade_case, upgrade_case, after_upgrade_case, before_downgrade_case, downgrade_case, after_downgrade_case]}].

all() ->
    [{group, upgrade_downgrade}].

suite() ->
    [
        {require, old_version},
        {require, new_version},
        {require, release_name},
        {require, release_dir}
    ].

init_per_suite(Config) ->
    ct:print("Initializing suite..."),
    ct:log(info, ?LOW_IMPORTANCE, "Initializing suite...", []),
    Docker = os:find_executable("docker"),

    OldVSN = ct:get_config(old_version),
    NewVSN = ct:get_config(new_version),
    ReleaseName = ct:get_config(release_name),
    ReleaseDir = ct:get_config(release_dir),
    docker_helper:build_image({OldVSN, NewVSN}, ReleaseName, ReleaseDir),

    %% start two Docker containers
    {ok, Peer, Node} = peer:start(#{name => ReleaseName,
        connection => standard_io,
        exec => {Docker, ["run", "-h", "one", "-i", ReleaseName]}}),

    {ok, Peer2, Node2} = peer:start(#{name => ReleaseName,
        connection => standard_io,
        exec => {Docker, ["run", "-h", "two", "-i", ReleaseName]}}),

    %% find IP address of the second node using alternative connection RPC
    {ok, Ips} = peer:call(Peer2, inet, getifaddrs, []),
    {"eth0", Eth0} = lists:keyfind("eth0", 1, Ips),
    {addr, Ip} = lists:keyfind(addr, 1, Eth0),

    %% make first node to discover second one
    ok = peer:call(Peer, inet_db, set_lookup, [[file]]),
    ok = peer:call(Peer, inet_db, add_host, [Ip, ["two"]]),

    %% join a cluster
    true = peer:call(Peer, net_kernel, connect_node, [Node2]),
    %% verify that second peer node has only the first node visible
    [Node] = peer:call(Peer2, erlang, nodes, []),

    [{peers, [Peer, Peer2]}, {nodes, [Node, Node2]} | Config].

end_per_suite(Config) ->
    Peers = ?config(peers, Config),
    [peer:stop(P) || P <- Peers].

% ========== CASES ==========

before_upgrade_case(_Config) ->
    ok.

upgrade_case(_Config) ->
    ok.
    % Peer = ?config(peer, Config),
    % NewVSN = ct:get_config(new_version),
    % OldVSN = ct:get_config(old_version),
    % ReleaseName = ct:get_config(release_name),
    % NewReleaseName = filename:join(NewVSN, ReleaseName),

    % {ok, NewVSN} = peer:call(Peer, release_handler, unpack_release, [NewReleaseName]),
    % {ok, OldVSN, _} = peer:call(Peer, release_handler, install_release, [NewVSN]),
    % ok = peer:call(Peer, release_handler, make_permanent, [NewVSN]),
    
    % Releases = peer:call(Peer, release_handler, which_releases, []),
    % ct:print("Installed releases:\n~p", [Releases]).

after_upgrade_case(_Config) ->
    ok.

before_downgrade_case(_Config) ->
    ok.

downgrade_case(_Config) ->
    ok.
    % Peer = ?config(peer, Config),
    % OldVSN = ct:get_config(old_version),

    % {ok, OldVSN, _} = peer:call(Peer, release_handler, install_release, [OldVSN]),
    % ok = peer:call(Peer, release_handler, make_permanent, [OldVSN]),

    % Releases = peer:call(Peer, release_handler, which_releases, []),
    % ct:print("Installed releases:\n~p", [Releases]).

after_downgrade_case(_Config) ->
    ok.
