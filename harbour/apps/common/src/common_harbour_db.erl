%%%-------------------------------------------------------------------
%%% @author Todd Greenwood-Geer
%%% @copyright (C) 2019, Enviro Software Solutions, LLC
%%% @doc
%%%
%%% @end
%%% Created : 2019-07-15 17:09:21.780311
%%%-------------------------------------------------------------------
-module(common_harbour_db).

-include_lib("stdlib/include/qlc.hrl").
-include("tables.hrl").
-include_lib("kernel/include/logger.hrl").

%% API
-export([create/1
        ,read/0
        ,read/1
        ,update/1
        ,delete/1
        ]).

-define(TABLE, ?TABLE_HARBOUR_REPORT_TASK).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Create and store items. 'C' of CRUD. Only store items that do not
%% yet exist.
%% @end
%%--------------------------------------------------------------------
-spec(create(Items :: [tuple()]) -> ok | {error, Reason :: term()}).
create(Items) ->
    Shazzam = erlang:system_time(second),
    CreatableItems = [X#?TABLE{date_created=Shazzam, date_modified=Shazzam} || X <- Items],
    F = fun() ->
            lists:foreach(fun mnesia:write/1, CreatableItems)
        end,
    case mnesia:transaction(F) of
        {atomic, ok} -> ok;
        {aborted, Reason} -> {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Return all records. 'R' of CRUD.
%% @end
%%--------------------------------------------------------------------
-spec(read() -> {ok, Values :: [term()]} | {error, Reason :: term()}).
read() ->
    read(fun(_) -> true end).

%%%--------------------------------------------------------------------
%%% @doc
%%% Return all 'enabled' records where FilterFun(X) =:= true. 'R' of CRUD.
%%% @end
%%%--------------------------------------------------------------------
-spec(read(F :: fun((X :: tuple()) -> boolean())) -> {ok, Values :: [term()]} | {error, Reason :: term()}).
read(F) ->
    case do(qlc:q(
              [X || X <-    mnesia:table(?TABLE), 
                            F(X) =:= true])) of
        {atomic, Values} -> {ok, Values};
        {aborted, Reason} -> {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Update records. 'U' of CRUD. 
%% @end
%%--------------------------------------------------------------------
-spec(update(Items :: [tuple()]) -> ok | {error, Reason :: term()}).
update(Items) ->
    Shazzam = erlang:system_time(second),
    UpdatableItems = [X#?TABLE{date_modified=Shazzam} || X <- Items],
    F = fun() ->
            lists:foreach(fun mnesia:write/1, UpdatableItems)
        end,
    case mnesia:transaction(F) of
        {atomic, ok} -> ok;
        {aborted, Reason} -> {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Delete an item. The 'D' of CRUD. 
%% @end
%%--------------------------------------------------------------------
-spec(delete(Items :: [tuple()]) -> ok | {error, Reason :: term()}).
delete(Items) ->
    F = fun() ->
            lists:foreach(fun mnesia:delete/1, Items)
        end,
    case mnesia:transaction(F) of
        {atomic, ok} -> ok;
        {aborted, Reason} -> {error, Reason}
    end.



-spec(do(Q :: any()) -> {atomic, Val :: [term()]} | {aborted, Reason :: term()}).
do(Q) ->
    F = fun() -> qlc:e(Q) end,
    mnesia:transaction(F).
