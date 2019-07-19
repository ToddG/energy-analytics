%%%-------------------------------------------------------------------
%%% @author Todd Greenwood-Geer
%%% @copyright (C) 2019, Enviro Software Solutions, LLC
%%% @doc
%%%
%%% @end
%%% Created : 2019-07-19 14:35:43.021999
%%%-------------------------------------------------------------------
-module(harbour_db).
-include("tables.hrl").
-include_lib("kernel/include/logger.hrl").

-export([tasks/0
        ,create/1
        ,update/1
        ,task/1
        ,tasks_in_state/1
        ,update_item_state/2
        ]).
         
-define(TABLE, ?TABLE_HARBOUR_REPORT_TASK).
%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------

tasks() ->
    common_harbour_db:read().

create(Tasks) ->
    common_harbour_db:create(Tasks).

update(Tasks) ->
    common_harbour_db:update(Tasks).

task(Url) ->
    common_harbour_db:read(fun(X) -> X#?TABLE.url =:= Url end).

tasks_in_state(State) ->
    common_harbour_db:read(fun(X) -> X#?TABLE.state =:= State end).

update_item_state(Item, NextState) ->
    Item1 = Item#?TABLE_HARBOUR_REPORT_TASK{state = NextState},
    common_harbour_db:update([Item1]).
