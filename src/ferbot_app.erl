%%%-------------------------------------------------------------------
%% @doc ferbot public API
%% @end
%%%-------------------------------------------------------------------

-module(ferbot_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->

    Dispatch = cowboy_router:compile(
                 [{'_', [
                         {"/", cowboy_static, {priv_file, ferbot, "index.html"}},
                         {"/static/[...]", cowboy_static, {priv_dir, ferbot, "static"}},
                         {"/websocket", ws_handler, []}
                        ]}]
                ),

    {ok, _} = cowboy:start_clear(http, [{port, 1122}],
                                        #{
                                          env => #{dispatch => Dispatch}
                                         }),
    
    ferbot_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
