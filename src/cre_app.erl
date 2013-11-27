-module(cre_app).

-behaviour(application).
-export([start/2,stop/1]).

%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for cre_app
start(_Type, _StartArgs) ->
    [webmachine_router:add_route(R) || R <- dispatch_table()],
    {ok, self()}.

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for cre_app.
stop(_State) ->
    ok.

%%% Internal Functions

dispatch_table() ->
    [
     {["cre", bucket, keys], cre_wm_multi, []}
    ].
