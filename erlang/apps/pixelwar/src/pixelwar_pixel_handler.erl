-module(pixelwar_pixel_handler).

-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2]).

init(Req0, State) ->
    {cowboy_websocket, Req0, State}.

websocket_init(State) ->
    gproc:reg({p, l, my_room}),
    {ok, State}.

websocket_handle({binary, <<_:8>>}, State) ->
    MatrixState = pixelwar_matrix_serv:get_state(matrix),
    {reply, [{binary, MatrixState}], State};
websocket_handle(
    {binary,
        <<X:1/little-unsigned-integer-unit:16, Y:1/little-unsigned-integer-unit:16,
            Color:1/little-unsigned-integer-unit:16>> = Data},
    State
) ->
    pixelwar_matrix_serv:set_element(matrix, {X, Y, Color}),
    gproc:send({p, l, my_room}, {self(), my_room, Data}),
    {ok, State}.

websocket_info({From, my_room, Event}, State) ->
    if
        From =:= self() -> {ok, State};
        true -> {reply, [{binary, Event}], State}
    end;
websocket_info(_Info, State) ->
    {stop, State}.
