-module(pixelwar_matrix_serv).

-behaviour(gen_server).

%% API
-export([start_link/1, set_element/2, get_state/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(pixelwar_matrix, {pixels, width, height}).

start_link(Args) ->
    gen_server:start_link({local, matrix}, ?MODULE, Args, []).

set_element(Instance, Pixel) ->
    gen_server:cast(Instance, {set_element, Pixel}).

get_state(Instance) ->
    gen_server:call(Instance, get_state).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init({Width, Height}) ->
    {ok, #pixelwar_matrix{pixels=#{}, width=Width, height=Height}}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(get_state, _From, State) ->
    ToBinary = fun (K, V, Acc) ->
        {X, Y} = K,
        <<Acc/binary, X:16/little, Y:16/little, V:16/little>>
    end,
    AsBinary = maps:fold(ToBinary, <<>>, State#pixelwar_matrix.pixels),
    {reply, AsBinary, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({set_element, {X, Y, Color}}, State) ->
    Key = {X, Y},
    if
        X >= State#pixelwar_matrix.width orelse X < 0 -> {noreply, State};
        Y >= State#pixelwar_matrix.height orelse Y < 0 -> {noreply, State};
        true ->
            NewPixels = maps:put(Key, Color, State#pixelwar_matrix.pixels),
            NewState = State#pixelwar_matrix{pixels=NewPixels, width=State#pixelwar_matrix.width, height=State#pixelwar_matrix.height},
            {noreply, NewState}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change("0.1.0", State, _Extra) ->
    logger:info("Code change ! ~w", [State]),
    Width = 128,
    Height = 128,
    IsInBound = fun({X, Y}, _V) -> X < Width andalso X >= 0 andalso Y < Height andalso Y >= 0 end,
    FilteredState = maps:filter(IsInBound, State),
    NewState = #pixelwar_matrix{pixels=FilteredState, width=Width, height=Height},
    logger:info("New state ! ~w", [NewState]),
    {ok, NewState};

code_change({down, "0.1.0"}, State, _Extra) ->
    {ok, State#pixelwar_matrix.pixels};

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
