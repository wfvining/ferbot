-module(ws_handler).

-export([init/2,
         websocket_init/1,
         websocket_handle/2,
         websocket_info/2]).

init(Req, Opts) ->
    {cowboy_websocket, Req, Opts}.

websocket_init(State) ->
    Port = open_port("priv/drive_port", {packet, 2}),
    {ok, Port}.

websocket_handle({text, Msg}, Port) -> 
    % send a message to the port controlling the robot.
    Port ! { self(), {command, Msg}},
    {ok, Port}.

websocket_info(_Info, State) ->
    {ok, State}.
