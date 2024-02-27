-module(pixelwar_ws_SUITE).
-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

all() ->
    [receive_state_test_case, send_pixel_test_case, send_pixel_get_state_test_case].

init_per_testcase(_Case, Config) ->
    application:load(pixelwar),
    application:set_env(pixelwar, matrix_width, 128),
    application:set_env(pixelwar, matrix_height, 128),

    application:load(gun),
    {ok, Apps} = application:ensure_all_started([pixelwar, gun]),
    [{apps, Apps} | Config].

end_per_testcase(_Case, Config) ->
    [application:stop(App) || App <- lists:reverse(?config(apps, Config))],
    Config.

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
    after 200 -> ?assertEqual(timeout, ExpectedFrame)
    end.

receive_state_test_case() ->
    [
        {doc, "Tries to receive the current state of the matrix"},
        {timetrap, timer:seconds(5)}
    ].
receive_state_test_case(_Config) ->
    {Pid, Ref, StreamRef} = wsConnect(),
    gun:ws_send(Pid, StreamRef, {binary, <<1:8>>}),
    listener(Pid, {binary, <<>>}),
    wsClose(Pid, Ref),
    ok.

send_pixel_test_case() ->
    [
        {doc, "Tries to send a pixel and then verifies that we receive nothing"},
        {timetrap, timer:seconds(5)}
    ].

send_pixel_test_case(_Config) ->
    {Pid, Ref, StreamRef} = wsConnect(),
    gun:ws_send(Pid, StreamRef, {binary, <<42:16, 24:16, 1024:16>>}),
    listener(Pid, timeout),
    wsClose(Pid, Ref),
    ok.

send_pixel_get_state_test_case() ->
    [
        {doc, "Tries to send a pixel and then verifies the current state"},
        {timetrap, timer:seconds(5)}
    ].

send_pixel_get_state_test_case(_Config) ->
    {Pid, Ref, StreamRef} = wsConnect(),
    gun:ws_send(Pid, StreamRef, {binary, <<42:16/little, 24:16/little, 1024:16/little>>}),
    gun:ws_send(Pid, StreamRef, {binary, <<1:8>>}),
    listener(Pid, {binary, <<42:16/little, 24:16/little, 1024:16/little>>}),
    wsClose(Pid, Ref),
    ok.
