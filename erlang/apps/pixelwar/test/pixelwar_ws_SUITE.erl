-module(pixelwar_ws_SUITE).
-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

all() ->
    [websocket_receive_state_test_case].

init_per_testcase(_Case, Config) ->
    application:load(pixelwar),
    application:load(gun),
    {ok, Apps} = application:ensure_all_started([pixelwar, gun]),
    [{apps, Apps} | Config].

end_per_testcase(_Case, Config) ->
    [application:stop(App) || App <- lists:reverse(?config(apps, Config))],
    Config.

conn() ->
    [{doc, "A connection is established and responds to packets"},
     {timetrap, timer:seconds(5)}].

wsConnect() ->
    {ok, Pid} = gun:open("localhost", 8080),
    {ok, http} = gun:await_up(Pid),
    Ref = monitor(process, Pid),
    gun:ws_upgrade(Pid, "/pixel", [], #{compress => true}),
    receive
        {gun_upgrade, Pid, StreamRef, _, _} ->
            {Pid, Ref, StreamRef};
        Msg ->
            ct:print("Unexpected message ~p", [Msg]),
            error(failed)
    end.

wsClose(Pid, Ref) ->
    demonitor(Ref),
    gun:close(Pid),
    gun:flush(Pid).

listener(Pid, ExpectedFrame) ->
    receive
        {gun_ws, Pid, _Ref, ReceivedFrame} ->
            ct:print("Received ~p", [ReceivedFrame]),
            ?assertEqual(ExpectedFrame, ReceivedFrame);
        Msg ->
            ct:print("Unexpected message ~p", [Msg])
    end.

websocket_receive_state_test_case() ->
    [{doc, "Tries to receive the current state of the matrix"},
     {timetrap, timer:seconds(5)}].
websocket_receive_state_test_case(_Config) ->
    {Pid, Ref, StreamRef} = wsConnect(),
    gun:ws_send(Pid, StreamRef, {binary, <<1:8>>}),
    listener(Pid, {binary,<<>>}),
    wsClose(Pid, Ref),
    ok.
