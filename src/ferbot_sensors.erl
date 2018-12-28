-module(ferbot_sensors).

-export([start/0, read_proximity/1, read_ambient/1]).

-define(LEFT,   1).
-define(RIGHT,  0).
-define(CENTER, 2).
-define(BUS,    "i2c-1").

-record(sensors, {mux, prox_left, prox_right, prox_center}).

start() ->
    {ok, Mux} = tca9548a:start(?BUS),
    {ok, ProxLeft}   = start_prox(?LEFT,   Mux),
    {ok, ProxRight}  = start_prox(?RIGHT,  Mux),
    {ok, ProxCenter} = start_prox(?CENTER, Mux),
    #sensors{mux         = Mux, 
             prox_left   = ProxLeft, 
             prox_right  = ProxRight, 
             prox_center = ProxCenter}.

start_prox(N, Mux) ->
    tca9548a:select(Mux, N),
    vcnl4010:start(?BUS).

read_proximity(Sensors=#sensors{mux=Mux}) ->
    tca9548a:select(Mux, ?LEFT),
    LeftProx = vcnl4010:read_proximity(Sensors#sensors.prox_left),

    tca9548a:select(Mux, ?RIGHT),
    RightProx = vcnl4010:read_proximity(Sensors#sensors.prox_right),

    tca9548a:select(Mux, ?CENTER),
    CenterProx = vcnl4010:read_proximity(Sensors#sensors.prox_center),

    [LeftProx, RightProx, CenterProx].

read_ambient(Sensors=#sensors{mux=Mux}) ->
    tca9548a:select(Mux, ?LEFT),
    LeftAmbient = vcnl4010:read_ambient(Sensors#sensors.prox_left),

    tca9548a:select(Mux, ?RIGHT),
    RightAmbient = vcnl4010:read_ambient(Sensors#sensors.prox_right),

    tca9548a:select(Mux, ?CENTER),
    CenterAmbient = vcnl4010:read_ambient(Sensors#sensors.prox_center),

    [LeftAmbient, RightAmbient, CenterAmbient].
