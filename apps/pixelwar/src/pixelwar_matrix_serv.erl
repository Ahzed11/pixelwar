-module(pixelwar_matrix_serv).
-vsn("0.3.0").
-behaviour(gen_server).
-include_lib("matrix.hrl").

-record(state, {
    matrix = #matrix{}
}).

%% API
-export([start_link/1, set_element/2, get_state/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link(ChannelName) ->
    gen_server:start_link({global, ChannelName}, ?MODULE, [], []).

set_element(ChannelName, Pixel) ->
    gen_server:cast({global, ChannelName}, {set_element, Pixel}).

get_state(ChannelName) ->
    gen_server:call({global, ChannelName}, get_state).

stop(ChannelName) -> gen_server:call({global, ChannelName}, stop).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(_Args) ->
    {ok, Matrix} = pixelwar_matrix:create(),
    {ok, #state{matrix = Matrix}}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(get_state, _From, State) ->
    Binary = pixelwar_matrix:to_binary(State#state.matrix),
    {reply, Binary, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({set_element, {X, Y, Color}}, State) ->
    case pixelwar_matrix:set_pixel(State#state.matrix, X, Y, Color) of
        {ok, NewMatrix} ->
            NewState = State#state{matrix=NewMatrix},
            {noreply, NewState};
        _ -> {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(normal, _State) ->
    ok;

terminate(_Reason, _State) ->
    ok.

code_change("0.2.0", State, _Extra) ->
    {_, Pixels, Width, Height} = State,
    {ok, Matrix} = pixelwar_matrix:create(Width, Height, Pixels),
    {ok, #state{matrix = Matrix}};

code_change({down, "0.2.0"}, State, _Extra) ->
    {_, {_, Pixels, Width, Height}} = State,
    {ok, {state, Pixels, Width, Height}};

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
