-module(vcnl4010).

-export([set_led_intensity/2, set_frequency/2, start/1, product_id/1,
         read_ambient/1, read_proximity/1]).

-define(VCNL4010_ADDR,     16#13).

-define(COMMAND,           16#80).
-define(PRODUCTID,         16#81).
-define(PROXRATE,          16#82).
-define(IRLED,             16#83).
-define(AMBIENTPARAMETER,  16#84). 
-define(AMBIENTDATA,       16#85).
-define(PROXIMITYDATA,     16#87).
-define(INTCONTROL,        16#89).
-define(PROXIMITYADJUST,   16#8a).
-define(INTSTAT,           16#8e).
-define(MODTIMING,         16#8f).
-define(MEASURE_AMBIENT,   16#10).
-define(MEASURE_PROXIMITY, 16#08).
-define(AMBIENT_READY,     16#40).
-define(PROXIMITY_READY,   16#20).

-type i2c_server_ref() :: pid().

-spec start(string()) -> {ok, i2c_server_ref()}.
start(Bus) ->
    {ok, Device} = i2c:start_link(Bus, ?VCNL4010_ADDR),
    true = verify_product_id(Device),
    ok = set_led_intensity(Device, 20),
    ok = set_frequency(Device, 62.5),
    ok = i2c:write(Device, <<?INTCONTROL:8,16#08>>),
    {ok, Device}.

stop(Device) ->
    i2c:stop(Device).

%% Translate a frequency in samples per second to the corresponding
%% code used by the device.
-spec frequency(float()) -> integer().
frequency(1.95)    -> 0;
frequency(3.90625) -> 1;
frequency(7.8125)  -> 2;
frequency(16.625)  -> 3;
frequency(31.25)   -> 4;
frequency(62.5)    -> 5;
frequency(125.0)   -> 6;
frequency(250.0)   -> 7;
frequency(_)       -> erlang:error(badarg).

%% Set the sample frequency
-spec set_frequency(i2c_server_ref(), float()) -> ok.
set_frequency(Device, Freq) ->
    F = frequency(Freq),
    i2c:write(Device, <<?MODTIMING:8,F:8>>).

%% Set the LED intensity, maximum of 20, any values larger than 20
%% will be clipped.
-spec set_led_intensity(i2c_server_ref(), integer()) -> ok.
set_led_intensity(Device, I) when I > 20 ->
    set_led_intensity(Device, 20);
set_led_intensity(Device, I) ->
    i2c:write(Device, <<?IRLED:8,I:8>>).

verify_product_id(Device) ->
    X = product_id(Device),
    (X band 16#f0) == 16#20.

product_id(Device) ->
    read_int8(Device, ?PRODUCTID).

read_ambient(Device) ->
    I = read_int8(Device, ?INTSTAT),
    ok = write_to(Device, ?INTSTAT, <<(I band (bnot 16#40)):8>>),
    ok = write_to(Device, ?COMMAND, <<?MEASURE_AMBIENT:8>>),
    do_when_ready(Device, ?AMBIENT_READY, 
                  fun () -> read_int16(Device, ?AMBIENTDATA) end).

read_proximity(Device) ->
    I = read_int8(Device, ?INTSTAT),
    ok = write_to(Device, ?INTSTAT, <<(I band (bnot 16#80)):8>>),
    ok = write_to(Device, ?COMMAND, <<?MEASURE_PROXIMITY:8>>),
    do_when_ready(Device, ?PROXIMITY_READY, 
                  fun () -> read_int16(Device, ?PROXIMITYDATA) end).

write_to(Device, Address, Data) ->
    i2c:write(Device, <<Address:8,Data/binary>>).

do_when_ready(Device, ReadyCode, F) ->
    i2c:write(Device, <<?COMMAND:8>>),
    <<Result>> = i2c:read(Device, 1),
    if (Result band ReadyCode) > 0 ->
            F();
       true ->
            timer:sleep(1),
            do_when_ready(Device, ReadyCode, F)
    end.

read_int16(Device, From) ->
    i2c:write(Device, <<From:8>>),
    <<High,Low>> = i2c:read(Device, 2),
    (High bsl 8) bor Low.

read_int8(Device, From) ->
    i2c:write(Device, <<From:8>>),
    binary:first(i2c:read(Device, 1)).
