%%%-------------------------------------------------------------------
%% @doc lct6_cache public API
%% @end
%%%-------------------------------------------------------------------

-module(lct6_cache_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    lct6_cache_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
