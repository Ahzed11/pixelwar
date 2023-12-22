-module(pixelwar_matrix_serv).
-behaviour(gen_server).

-record(state, {
    pixels = #{} :: #{{non_neg_integer(), non_neg_integer()} => non_neg_integer()},
    width = 128 :: non_neg_integer(),
    height = 128 :: non_neg_integer()
}).

-state_record(state).

%% API
-export([start_link/1, set_element/2, get_state/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link(Args) ->
    gen_server:start_link({local, matrix}, ?MODULE, Args, []).

set_element(Instance, Pixel) ->
    gen_server:cast(Instance, {set_element, Pixel}).

get_state(Instance) ->
    gen_server:call(Instance, get_state).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init({Width, Height}) ->
    {ok, #state{pixels=#{}, width=Width, height=Height}}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(get_state, _From, State) ->
    ToBinary = fun (K, V, Acc) ->
        {X, Y} = K,
        <<Acc/binary, X:16/little, Y:16/little, V:16/little>>
    end,
    AsBinary = maps:fold(ToBinary, <<>>, State#state.pixels),
    {reply, AsBinary, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({set_element, {X, Y, Color}}, State) ->
    Key = {X, Y},
    if
        X >= State#state.width orelse X < 0 -> {noreply, State};
        Y >= State#state.height orelse Y < 0 -> {noreply, State};
        true ->
            NewPixels = maps:put(Key, Color, State#state.pixels),
            NewState = State#state{pixels=NewPixels, width=State#state.width, height=State#state.height},
            {noreply, NewState}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
