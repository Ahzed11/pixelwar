-module(pixelwar_matrix_serv).

-behaviour(gen_server).

%% API
-export([start_link/0, set_element/2, get_state/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, matrix}, ?MODULE, [], []).

set_element(Instance, Pixel) ->
    gen_server:cast(Instance, {set_element, Pixel}).

get_state(Instance) ->
    gen_server:call(Instance, get_state).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(_Args) ->
    {ok, #{}}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(get_state, _From, State) ->
    ToBinary = fun (K, V, Acc) ->
        {X, Y} = K,
        <<Acc/binary, X:16/little, Y:16/little, V:16/little>>
    end,
    AsBinary = maps:fold(ToBinary, <<>>, State),
    {reply, AsBinary, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({set_element, {X, Y, Color}}, State) ->
    Key = {X, Y},
    NewState = maps:put(Key, Color, State),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
