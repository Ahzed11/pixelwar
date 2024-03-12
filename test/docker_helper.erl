-module(docker_helper).
-export([build_image/3]).
-compile(export_all).

build_image({OldVSN, NewVSN}, ReleaseName, ReleaseDir) ->
    NewReleaseName = ReleaseName ++ "-" ++ NewVSN,
    OldReleaseName = ReleaseName ++ "-" ++ OldVSN,

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
        "RUN mkdir /opt/" ++ ReleaseName ++ "/releases/" ++ NewVSN ++ "\n"
        "RUN cp /tmp/" ++ NewReleaseName ++ ".tar.gz /opt/" ++ ReleaseName ++ "/releases/" ++ NewVSN ++ "/" ++ ReleaseName ++ ".tar.gz\n"
        "ENTRYPOINT [\"/opt/" ++ ReleaseName ++ "/erts-" ++ erlang:system_info(version) ++
        "/bin/dyn_erl\", \"-boot\", \"/opt/" ++ ReleaseName ++ "/releases/" ++ OldVSN ++ "/start\","
        " \"-kernel\", \"inet_dist_listen_min\", \"4445\","
        " \"-erl_epmd_port\", \"4445\","
        " \"-setcookie\", \"secret\"]\n",
    ok = file:write_file(BuildScript, Dockerfile),
    os:cmd("docker build -t " ++ ReleaseName ++ " .").