-module(pixelwar_pixel_handler).

-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2]).

-record(state, {
    server_name :: binary
}).

init(Req, _State) ->
    ServerName = cowboy_req:binding(room, Req),
    pixelwar_sup:add_matrix(ServerName),
    {cowboy_websocket, Req, #state{server_name=ServerName}}.

websocket_init(State) ->
    gproc:ensure_reg({p, l, State#state.server_name}),
    {ok, State}.

websocket_handle({binary, <<_:8>>}, State) ->
    MatrixState = pixelwar_matrix_serv:get_state(State#state.server_name),
    {reply, [{binary, MatrixState}], State};
websocket_handle(
    {binary,
        <<X:1/little-unsigned-integer-unit:16, Y:1/little-unsigned-integer-unit:16,
            Color:1/little-unsigned-integer-unit:16>> = Data},
    State
) ->
    pixelwar_matrix_serv:set_element(State#state.server_name, {X, Y, Color}),
    gproc:send({p, l, State#state.server_name}, {self(), State#state.server_name, Data}),
    {ok, State}.

websocket_info({From, Room, Event}, State) when Room =:= State#state.server_name ->
    if
        From =:= self() -> {ok, State};
        true -> {reply, [{binary, Event}], State}
    end;
websocket_info(_Info, State) ->
    {ok, State}.
