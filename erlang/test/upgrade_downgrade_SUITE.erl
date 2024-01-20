-module(upgrade_downgrade_SUITE).
-behaviour(ct_suite).
-export([all/0, groups/0]).
-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

groups() ->
    [{upgrade_downgrade, [sequence], [before_upgrade_case, upgrade_case, downgrade_case]}].

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
    Docker = os:find_executable("docker"),
    build_image(),
    ReleaseName = ct:get_config(release_name),

    {ok, Peer, Node} = peer:start(#{name => ReleaseName,
        connection => standard_io,
        exec => {Docker, ["run", "-h", "one", "-i", ReleaseName]}}),

    [{peer, Peer}, {node, Node} | Config].

end_per_suite(Config) ->
    Peer = ?config(peer, Config),
    peer:stop(Peer).

% ========== CASES ==========

before_upgrade_case(Config) ->
    Peer = ?config(peer, Config),

    peer:call(Peer, pixelwar_matrix_serv, set_element, [matrix, {12, 12, 12}]),
    peer:call(Peer, pixelwar_matrix_serv, set_element, [matrix, {222, 222, 222}]),
    
    MatrixAsBin = peer:call(Peer, pixelwar_matrix_serv, get_state, [matrix]),
    ?assertEqual(
        MatrixAsBin,
        <<12:16/little, 12:16/little, 12:16/little, 222:16/little, 222:16/little, 222:16/little>>
    ).

upgrade_case(Config) ->
    Peer = ?config(peer, Config),
    NewVSN = ct:get_config(new_version),
    OldVSN = ct:get_config(old_version),
    ReleaseName = ct:get_config(release_name),
    NewReleaseName = filename:join(NewVSN, ReleaseName),

    {ok, NewVSN} = peer:call(Peer, release_handler, unpack_release, [NewReleaseName]),
    {ok, OldVSN, _} = peer:call(Peer, release_handler, install_release, [NewVSN]),
    ok = peer:call(Peer, release_handler, make_permanent, [NewVSN]),
    
    Releases = peer:call(Peer, release_handler, which_releases, []),
    ct:print("Installed releases:\n~p", [Releases]).

downgrade_case(Config) ->
    Peer = ?config(peer, Config),
    OldVSN = ct:get_config(old_version),

    {ok, OldVSN, _} = peer:call(Peer, release_handler, install_release, [OldVSN]),
    ok = peer:call(Peer, release_handler, make_permanent, [OldVSN]).

% ========== HELPERS ==========

build_image() ->
    NewVSN = ct:get_config(new_version),
    OldVSN = ct:get_config(old_version),
    ReleaseName = ct:get_config(release_name),
    NewReleaseName = ReleaseName ++ "-" ++ NewVSN,
    OldReleaseName = ReleaseName ++ "-" ++ OldVSN,
    ReleaseDir = ct:get_config(release_dir),

    NewReleasePath = filename:join(ReleaseDir, NewReleaseName ++ ".tar.gz"),
    file:copy(NewReleasePath, "./" ++ NewReleaseName ++ ".tar.gz"),

    OldReleasePath = filename:join(ReleaseDir, OldReleaseName ++ ".tar.gz"),
    file:copy(OldReleasePath, "./" ++ OldReleaseName ++ ".tar.gz"),

    %% Create Dockerfile example, working only for Ubuntu 20.04
    %% Expose port 4445, and make Erlang distribution to listen
    %% on this port, and connect to it without EPMD
    %% Set cookie on both nodes to be the same.
    BuildScript = filename:join("./", "Dockerfile"),
    Dockerfile =
        "FROM ubuntu:22.04 as runner\n"
        "EXPOSE 4445\n"
        "WORKDIR /opt/" ++ ReleaseName ++  "\n"
        "COPY [\"" ++ OldReleaseName ++ ".tar.gz\", \"" ++ NewReleaseName ++ ".tar.gz\"" ++ ", \"/tmp/\"]\n"
        "RUN tar -zxvf /tmp/" ++ OldReleaseName ++ ".tar.gz -C /opt/" ++ ReleaseName ++ "\n"
        "RUN mkdir /opt/pixelwar/releases/" ++ NewVSN ++ "\n"
        "RUN cp /tmp/" ++ NewReleaseName ++ ".tar.gz /opt/" ++ ReleaseName ++ "/releases/" ++ NewVSN ++ "/" ++ ReleaseName ++ ".tar.gz\n"
        "ENTRYPOINT [\"/opt/" ++ ReleaseName ++ "/erts-" ++ erlang:system_info(version) ++
        "/bin/dyn_erl\", \"-boot\", \"/opt/" ++ ReleaseName ++ "/releases/" ++ OldVSN ++ "/start\","
        " \"-kernel\", \"inet_dist_listen_min\", \"4445\","
        " \"-erl_epmd_port\", \"4445\","
        " \"-setcookie\", \"secret\"]\n",
    ok = file:write_file(BuildScript, Dockerfile),
    DockerBuildResult = os:cmd("docker build -t " ++ ReleaseName ++ " ."),
    ct:print("Docker build:\n~s", [DockerBuildResult]).