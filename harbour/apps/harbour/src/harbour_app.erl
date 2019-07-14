%%%-------------------------------------------------------------------
%% @doc harbour public API
%% @end
%%%-------------------------------------------------------------------

-module(harbour_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    harbour_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
