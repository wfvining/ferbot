-module(tca9548a).

-export([start/1, start/2, stop/1, select/2]).

-define(TCA9548A_ADDR, 16#70).

start(Bus) ->
    i2c:start_link(Bus, ?TCA9548A_ADDR).

start(Bus, Address) ->
    i2c:start_link(Bus, Address).

stop(Device) ->
    i2c:stop(Device).

select(_, Channel) when (Channel < 0) or (Channel > 8) ->
    erlang:error(badarg);
select(Device, Channel) ->
    i2c:write(Device, <<(1 bsl Channel):8>>).
