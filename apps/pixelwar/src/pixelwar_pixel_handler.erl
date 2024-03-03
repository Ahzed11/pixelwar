-module(pixelwar_pixel_handler).

-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2]).

-record(state, {
    channel_name :: binary
}).

init(Req, _State) ->
    ChannelName = cowboy_req:binding(room, Req),
    pixelwar_sup:add_matrix(ChannelName),
    {cowboy_websocket, Req, #state{channel_name=ChannelName}}.

websocket_init(State) ->
    gproc:reg({p, l, State#state.channel_name}),
    {ok, State}.

websocket_handle({binary, <<_:8>>}, State) ->
    MatrixState = pixelwar_matrix_serv:get_state(State#state.channel_name),
    {reply, [{binary, MatrixState}], State};
websocket_handle(
    {binary,
        <<X:1/little-unsigned-integer-unit:16, Y:1/little-unsigned-integer-unit:16,
            Color:1/little-unsigned-integer-unit:16>> = Data},
    State
) ->
    pixelwar_matrix_serv:set_element(State#state.channel_name, {X, Y, Color}),
    gproc:send({p, l, State#state.channel_name}, {self(), State#state.channel_name, Data}),
    {ok, State}.

websocket_info({From, Room, Event}, State) when Room =:= State#state.channel_name ->
    if
        From =:= self() -> {ok, State};
        true -> {reply, [{binary, Event}], State}
    end;
websocket_info(_Info, State) ->
    {stop, State}.
