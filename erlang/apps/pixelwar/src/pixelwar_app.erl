%%%-------------------------------------------------------------------
%% @doc pixelwar public API
%% @end
%%%-------------------------------------------------------------------

-module(pixelwar_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/pixel", pixelwar_pixel_handler, []}]}    
    ]),
    {ok, _} = cowboy:start_clear(my_http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    pixelwar_sup:start_link().

stop(_State) ->
    ok.
