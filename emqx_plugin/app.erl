%%%-------------------------------------------------------------------
%% @doc {{name}} public API
%% @end
%%%-------------------------------------------------------------------

-module({{name}}_app).
-include("{name}}_plugin.hrl").
-behaviour(application).

-emqx_plugin(?MODULE).

-export([ start/2
        , stop/1
        ]).

start(_StartType, _StartArgs) ->
    {ok, Sup} = {{name}}_sup:start_link(),
    ?APP:load(),
    {ok, Sup}.

stop(_State) ->
    ?APP:unload(),
    ok.
%% internal functions
