-module(pixelwar_matrix_serv).
-vsn("1.0.0").
-behaviour(gen_server).
-include_lib("matrix.hrl").

-record(state, {
    matrix = #matrix{}
}).

%% API
-export([start_link/1, set_element/2, get_state/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link(ServerName) ->
    gen_server:start_link({global, ServerName}, ?MODULE, [], []).

set_element(ServerName, Pixel) ->
    gen_server:cast({global, ServerName}, {set_element, Pixel}).

get_state(ServerName) ->
    gen_server:call({global, ServerName}, get_state).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(_Args) ->
    {ok, Matrix} = pixelwar_matrix:create(),
    {ok, #state{matrix = Matrix}}.

handle_call(stop, _From, State) ->
    {stop, normal, State};

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

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
