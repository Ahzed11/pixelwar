%%%-------------------------------------------------------------------
%% @doc pixelwar public API
%% @end
%%%-------------------------------------------------------------------

-module(pixelwar_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/:room", pixelwar_pixel_handler, []}]}
    ]),
    persistent_term:put(pixelwar_dispatch, Dispatch),
    {ok, _} = cowboy:start_clear(
        my_http_listener,
        [{port, 8080}],
        #{env => #{dispatch => {persistent_term, pixelwar_dispatch}}}
    ),
    
    pixelwar_sup:start_link().

stop(_State) ->
    ok.
