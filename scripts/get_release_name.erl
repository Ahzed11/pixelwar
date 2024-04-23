#!/usr/bin/env escript
-module(get_release_name).
-mode(compile).

usage() ->
    io:format("usage: RebarConfigPath\n"),
    halt(1).

main([Path]) ->
    {ok, Terms} = file:consult(Path),
    Filter = fun (E) ->
        case E of
            {relx, _} -> true;
            _ -> false
        end
    end,
    [H | _] = lists:filter(Filter, Terms),
    {relx, Relx} = H,
    {release, {Release, _}, _} = lists:nth(1, Relx),
    io:format("~w", [Release]);

main(_) ->
    usage().