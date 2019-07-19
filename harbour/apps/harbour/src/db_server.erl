%%%-------------------------------------------------------------------
%%% @author Todd Greenwood-Geer
%%% @copyright (C) 2019, Enviro Software Solutions, LLC
%%% @doc
%%%
%%% @end
%%% Created : 2019-07-15 17:09:21.780311
%%%-------------------------------------------------------------------
-module(db_server).

-behaviour(gen_server).
-include_lib("stdlib/include/qlc.hrl").
-include("../../common/include/tables.hrl").
-include_lib("kernel/include/logger.hrl").
%% API
-export([start_link/0
        ,create/1
        ,read/1
        ,read/2
        ,update/1
        ,delete/1
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Create and store items. 'C' of CRUD. Only store items that do not
%% yet exist.
%% @end
%%--------------------------------------------------------------------
-spec(create(Items :: [tuple()]) -> ok | {error, Reason :: term()}).
create(Items) ->
    ?LOG_DEBUG(#{server=>db, what=>create, items=>Items}),
    gen_server:call(?MODULE, {create, Items}). 

%%--------------------------------------------------------------------
%% @doc
%% Return all 'enabled' records. 'R' of CRUD.
%% @end
%%--------------------------------------------------------------------
-spec(read(Table :: atom()) -> {ok, [tuple()]} | {error, Reason :: term()}).
read(Table) ->
    ?LOG_DEBUG(#{server=>db, what=>read, table=>Table}),
    gen_server:call(?MODULE, {read, Table}). 

%%--------------------------------------------------------------------
%% @doc
%% Return all 'enabled' records where FilterFun(X) =:= true. 'R' of CRUD.
%% @end
%%--------------------------------------------------------------------
-spec(read(Table :: atom(), FilterFun :: fun((tuple()) -> boolean())) -> {ok, [tuple()]} | {error, Reason :: term()}).
read(Table, FilterFun) ->
    ?LOG_DEBUG(#{server=>db, what=>read, table=>Table}),
    gen_server:call(?MODULE, {read, Table, FilterFun}). 

%%--------------------------------------------------------------------
%% @doc
%% Update all 'enabled' records. 'U' of CRUD. Attempts to update
%% 'disabled' records will raise an error.
%% @end
%%--------------------------------------------------------------------
-spec(update(Items :: [tuple()]) -> ok | {error, Reason :: term()}).
update(Items) ->
    ?LOG_DEBUG(#{server=>db, what=>update, items=>Items}),
    gen_server:call(?MODULE, {update, Items}). 

%%--------------------------------------------------------------------
%% @doc
%% Delete an item. The 'D' of CRUD. When an item is deleted, it is not
%% really deleted. No-one does that. Instead, the record is marked
%% as 'disabled', meaning that it is to be treated as if it has
%% been deleted. 
%%
%% * 'disabled' items are returned in read operations
%% @end
%%--------------------------------------------------------------------
-spec(delete(Items :: [tuple()]) -> ok | {error, Reason :: term()}).
delete(Items) ->
    ?LOG_DEBUG(#{server=>db, what=>delete, items=>Items}),
    gen_server:call(?MODULE, {delete, Items}). 

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
        State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_call({create, Items}, _From, State) ->
    {reply, p_create(Items), State};
handle_call({read, Table, FilterFun}, _From, State) ->
    {reply, p_read(Table, FilterFun), State};
handle_call({read, Table}, _From, State) ->
    {reply, p_read(Table, fun (_) -> true end), State};
handle_call({update, Items}, _From, State) ->
    {reply, p_update(Items), State};
handle_call({delete, Items}, _From, State) ->
    {reply, p_delete(Items), State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
        State :: #state{}) -> term()).
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
        Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec(p_create(Items :: [tuple()]) -> ok | {error, Reason :: term()}).
p_create(Items) ->
    F = fun() ->
        ok = consistency_check(Items),
        Table = table(lists:nth(1, Items)),
        ExistingIdList = qlc:e(qlc:q([id(X) ||  X <- mnesia:table(Table)])),
        ExistingIdSet = sets:from_list(ExistingIdList),
        CreatableItems = lists:filter( fun (X) -> not sets:is_element(id(X), ExistingIdSet) end, Items),
        if 
            CreatableItems =/= Items -> 
                mnesia:abort(uncreatable_items);
            true -> 
                Shazzam = erlang:system_time(second),
                RecordStates = [#?TABLE_HARBOUR_RECORD_STATE{fkid=id(X), date_created=Shazzam, date_modified=Shazzam} || X <- CreatableItems],
                lists:foreach(fun mnesia:write/1, CreatableItems),
                lists:foreach(fun mnesia:write/1, RecordStates)
            end
        end,
    case mnesia:transaction(F) of
        {atomic, ok} -> ok;
        {aborted, Reason} -> {error, Reason}
    end.

-spec(p_read(Table :: term(), F :: fun((X :: tuple()) -> boolean())) -> {ok, Values :: [term()]} | {error, Reason :: term()}).
p_read(Table, F) ->
    case do(qlc:q([X || X <- mnesia:table(Table),
                        Y <- mnesia:table(?TABLE_HARBOUR_RECORD_STATE),
                        id(X) =:= id(Y),
                        F(X) =:= true,
                        Y#?TABLE_HARBOUR_RECORD_STATE.enabled =:= true])) of
        {atomic, Values} -> {ok, Values};
        {aborted, Reason} -> {error, Reason}
    end.

-spec(p_update(Items :: [tuple()]) -> ok | {error, Reason :: term()}).
p_update(Items) ->
    F = fun() ->
        ok = consistency_check(Items),
        Table = table(lists:nth(1, Items)),
        EnabledIdList = qlc:e(qlc:q([id(X) ||   X <- mnesia:table(Table),
                                                Y <- mnesia:table(?TABLE_HARBOUR_RECORD_STATE),
                                                id(X) =:= id(Y),
                                                Y#?TABLE_HARBOUR_RECORD_STATE.enabled =:= true])),
        EnabledIdSet = sets:from_list(EnabledIdList),
        UpdatableItems = lists:filter(fun (X) -> sets:is_element(id(X), EnabledIdSet) end, Items),
        if 
            UpdatableItems =/= Items -> 
                mnesia:abort(unupdatable_items);
            true -> 
                Shazzam = erlang:system_time(second),
                UpdatableItemIdList = [id(X) || X <- UpdatableItems],
                UpdatableItemIdSet = sets:from_list(UpdatableItemIdList),
                RecordStates = qlc:e(qlc:q([X ||    X <- mnesia:table(?TABLE_HARBOUR_RECORD_STATE),
                                                    sets:is_element(id(X), UpdatableItemIdSet)])),
                UpdatedRecordStates = [X#?TABLE_HARBOUR_RECORD_STATE{date_modified=Shazzam} || X <- RecordStates],
                lists:foreach(fun mnesia:write/1, UpdatableItems),
                lists:foreach(fun mnesia:write/1, UpdatedRecordStates)
            end
        end,
    case mnesia:transaction(F) of
        {atomic, ok} -> ok;
        {aborted, Reason} -> {error, Reason}
    end.

-spec(p_delete(Items :: [tuple()]) -> ok | {error, Reason :: term()}).
p_delete(Items) ->
    F = fun() ->
        ok = consistency_check(Items),
        Shazzam = erlang:system_time(second),
        ItemsIdList = [id(X) || X <- Items],
        ItemsIdSet = sets:from_list(ItemsIdList),
        ItemsRStates = qlc:e(qlc:q([X || X <-   mnesia:table(?TABLE_HARBOUR_RECORD_STATE),
                                                sets:is_element(X#?TABLE_HARBOUR_RECORD_STATE.fkid, ItemsIdSet),
                                                X#?TABLE_HARBOUR_RECORD_STATE.enabled =:= true
                                   ])),
        UpdatedRecordStates = [R#?TABLE_HARBOUR_RECORD_STATE{enabled=false, date_modified=Shazzam} || R <- ItemsRStates],
        lists:foreach(fun mnesia:write/1, UpdatedRecordStates)
    end,
    case mnesia:transaction(F) of
        {atomic, ok} -> ok;
        {aborted, Reason} -> {error, Reason}
    end.

-spec(do(Q :: any()) -> {atomic, Val :: [term()]} | {aborted, Reason :: term()}).
do(Q) ->
    F = fun() -> qlc:e(Q) end,
    mnesia:transaction(F).

-spec(consistency_check(Items :: [tuple()]) ->  ok | {aborted, Reason :: term()}).
consistency_check(Items) ->
    Tables = sets:to_list(sets:from_list([table(X) || X <- Items])),
    case Tables of
        [?TABLE_HARBOUR_RECORD_STATE] ->
            mnesia:abort(invalid_table);
        [_,_] ->
            mnesia:abort(too_many_tables);
        [_] -> 
            ok
    end.

-spec(table(R::tuple()) -> any()).
table(R) ->
    element(1, R).

-spec(id(R::tuple()) -> reference()).
id(R) ->
    element(2, R).
