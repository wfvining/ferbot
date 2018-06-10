-module(ws_handler).

-export([init/2,
         websocket_init/1,
         websocket_handle/2,
         websocket_info/2]).

init(Req, Opts) ->
    io:format("ws:init: ~p ~p~n", [Req,Opts]),
    {cowboy_websocket, Req, Opts}.

websocket_init(State) ->
    io:format("ws:ws_init: ~p", [State]),
    Port = open_port({spawn, "priv/drive_port"}, [{packet, 1}]),
    io:format("ws:ws_init: ~p", [Port]),
    {ok, Port}.

websocket_handle({text, Msg}, Port) -> 
    % send a message to the port controlling the robot.
    Port ! { self(), {command, Msg}},
    {ok, Port};
websocket_handle(_Frame, State) -> 
    {ok, State}.

websocket_info(_Info, State) ->
    {ok, State}.
