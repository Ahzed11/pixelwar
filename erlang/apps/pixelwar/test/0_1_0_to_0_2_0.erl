-module('0_1_0_to_0_2_0').
-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

groups() ->
    [
        {group1, [sequence], [add_pixel_case, update_state_case, state_after_update_case]}
    ].

all() ->
    [{group, group1}].

init_per_suite(_Case, Config) ->
    [{newVersion, "0.2.0"} | Config].

end_per_suite(_Case, Config) ->
    Config.

add_pixel_case() ->
    [{doc, "Puts pixel in and out of bounds in 0.1.0"},
     {timetrap, timer:seconds(5)}].

add_pixel_case(_Config) ->
    pixelwar_matrix_serv:set_element(matrix, {42, 42, 12}),
    pixelwar_matrix_serv:set_element(matrix, {2000, 2000, 2000}),

    MatrixAsBin = pixelwar_matrix_serv:get_state(matrix),
    ?assertEqual(MatrixAsBin, <<2000:16/little, 2000:16/little, 2000:16/little, 42:16/little, 42:16/little, 42:16/little>>).

update_state_case() ->
    [{doc, "Update to new version"},
     {timetrap, timer:seconds(5)}].

update_state_case(Config) ->
    NewVersion = ?config(newVersion, Config),
    os:cmd("relupci/bin/pixelwar unpack " ++ NewVersion),
    os:cmd("relupci/bin/pixelwar install " ++ NewVersion),
    os:cmd("relupci/bin/pixelwar upgrade " ++ NewVersion).

state_after_update_case() ->
    [{doc, "Check test after update"},
     {timetrap, timer:seconds(5)}].

state_after_update_case(_Config) ->
    MatrixAsBin = pixelwar_matrix_serv:get_state(matrix),
    ?assertEqual(MatrixAsBin, <<42:16/little, 42:16/little, 42:16/little>>).