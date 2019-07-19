%%%-------------------------------------------------------------------
%%% @author Todd Greenwood-Geer
%%% @copyright (C) 2019, Enviro Software Solutions, LLC
%%% @doc
%%%
%%% @end
%%% Created : 2019-07-15 08:06:09.865124
%%%-------------------------------------------------------------------
-module(work_server).

-behaviour(gen_server).
-include("../../common/include/tables.hrl").
-include_lib("kernel/include/logger.hrl").

%% API
-export([start_link/0
         ,refresh/0
         ,next_downloadable_item/0
         ,item_download_complete/1
         ,next_parsable_item/0
         ,item_parse_complete/1
%         ,next_archivable_item/0
%         ,item_archive_complete/1
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {config                  :: work_server_config()}).
-record(work_server_config, {
                             refresh    :: pos_integer()
                            }).
-type work_server_config()              :: #work_server_config{}.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%%--------------------------------------------------------------------
%% @doc
%% Calls -> manifest_server:manifests() to get the full set of current
%% manifests. After retrieving, adds the new items to the db table
%% `harbour_report_task`.
%% @end
%%--------------------------------------------------------------------
-spec(refresh() -> 
    ok | {error, Reason :: term()}).
refresh() ->
    ?LOG_INFO(#{service => work, what => refresh}), 
    gen_server:call(?MODULE, {refresh}).


%%--------------------------------------------------------------------
%% @doc
%% Returns the next item that hasn't been previously downloaded. 
%% Returns items sorted with current items first based on the report
%% timestamp.
%% @end
%%--------------------------------------------------------------------
-spec(next_downloadable_item() -> 
    {ok, Task :: harbour_report_task()} | {error, Reason :: term()}).
next_downloadable_item() ->
    ?LOG_INFO(#{service => work, what => next_downloadable_item}), 
    gen_server:call(?MODULE, {next_downloadable_item}).


%%--------------------------------------------------------------------
%% @doc
%% Register an item as having completed the download process.
%% @end
%%--------------------------------------------------------------------
-spec(item_download_complete(Id :: reference()) -> 
    ok | {error, Reason :: term()}).
item_download_complete(Id) ->
    ?LOG_INFO(#{service => work, what => item_download_complete, id=>Id}), 
    gen_server:call(?MODULE, {item_download_complete, Id}).

%%--------------------------------------------------------------------
%% @doc
%% Returns the next item that has been downloaded but not yet parsed. 
%% Returns items sorted with current items first based on the report
%% timestamp.
%% @end
%%--------------------------------------------------------------------
-spec(next_parsable_item() -> 
    {ok, Task :: harbour_report_task()} | {error, Reason :: term()}).
next_parsable_item() ->
    ?LOG_INFO(#{service => work, what => next_parsable_item}), 
    gen_server:call(?MODULE, {next_parsable_item}).


%%--------------------------------------------------------------------
%% @doc
%% Register an item as having completed the parse process.
%% @end
%%--------------------------------------------------------------------
-spec(item_parse_complete(Id :: reference()) ->
    ok | {error, Reason :: term()}).
item_parse_complete(Id) ->
    ?LOG_INFO(#{service => work, what => item_parse_complete, id=> Id}), 
    gen_server:call(?MODULE, {item_parse_complete, Id}).

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
    {ok, ConfigDir} = application:get_env(harbour, config_dir),
    ConfigFile = filename:join(ConfigDir, "work.config"),
    ConfigRecord = case file:consult(ConfigFile) of
        {ok, Terms} -> parse_config(Terms)
    end,
    State = #state{config=ConfigRecord},
    spawn_link(fun() -> cron_loop(State) end),
    {ok, State}.

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
handle_call({refresh}, _From, State) ->
    {reply, refresh_reports(), State};
handle_call({next_downloadable_item}, _From, State) ->
    {reply, get_next_downloadable_item(), State};
handle_call({item_download_complete, Id}, _From, State) ->
    {reply, update_item_state(Id, download_started, download_completed), State};
handle_call({next_parsable_item}, _From, State) ->
    {reply, get_next_parsable_item(), State};
handle_call({item_parse_complete, Id}, _From, State) ->
    {reply, update_item_state(Id, parse_started, parse_completed), State};
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
handle_info({pubsub, {work_server_config, _} = C}, State) ->
    ConfigRecord = parse_config([C]),
    State1 = State#state{config=ConfigRecord},
    ?LOG_INFO(#{service => work, what => handle_info, pubsub => work_server_config, state0 => State, state1 => State1}), 
    {noreply, State1};
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Parse the list of terms, which could be anything, into a valid
%% work server config record.
%%
%%
%% Expected config from ./config/work.config. Could be delivered either
%% by file:consult or by a message bus event from a file change 
%% notification.
%%
%% {work_server_config, [
%%%         {database_name, "work"},
%%%         {database_path, "data"},
%%%         {manifest_refresh_frequency_ms, 3600000},
%%%     ]
%%% }.
%%%
%%% TODO: these config values are bogus
%%%
%% @end
%%--------------------------------------------------------------------
-spec(parse_config([term()]) -> #work_server_config{}).
parse_config(ConfigList) ->
    Config      = proplists:get_value(work_server_config, ConfigList),
    Refresh     = proplists:get_value(refresh, Config),
    #work_server_config{refresh=Refresh}.    


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get the disjoint of the current urls and the new urls and add them
%% to the database.
%% @end
%%--------------------------------------------------------------------
-spec(refresh_reports() -> ok | {error, Reason :: term()}).
refresh_reports() ->
    {ok, Manifests} = manifest_server:manifests(),
    ?LOG_INFO(#{service => work, what => refresh_reports, manifests_count=>length(Manifests)}), 
    {ok, Tasks} = db_server:read(?TABLE_HARBOUR_REPORT_TASK),
    ?LOG_INFO(#{service => work, what => refresh_reports, tasks_count=>length(Tasks)}), 
    Existing = [{X#?TABLE_HARBOUR_REPORT_TASK.url, X#?TABLE_HARBOUR_REPORT_TASK.filename} || X <- Tasks],
    New = lists:subtract(Manifests, Existing),
    NewTasks = [#?TABLE_HARBOUR_REPORT_TASK{id=make_ref(), url=Url, filename=File} || {Url, File} <- New],
    ?LOG_INFO(#{service => work, what => refresh_reports, new_tasks_count=>length(NewTasks)}), 
    ok = db_server:create(NewTasks),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec(get_next_downloadable_item() -> {ok, Item::harbour_report_task()} | {error, Reason :: term()}).
get_next_downloadable_item() -> 
    transition_next_item(undefined, download_started).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec(get_next_parsable_item() -> {ok, Item::harbour_report_task()} | {error, Reason :: term()}).
get_next_parsable_item() ->
    transition_next_item(download_completed, parse_started).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% TODO: think about how to better expose item state.
%% @end
%%--------------------------------------------------------------------
-spec(update_item_state(Id :: reference(), 
                        PrevState :: harbour_report_task_state(),
                        NextState :: harbour_report_task_state()) 
      -> {ok, Item :: tuple()} | {error, Reason :: term()}).
update_item_state(Id, PrevState, NextState) ->
    ?LOG_INFO(#{service => work, what => update_item_state, id => Id, state0 => PrevState, state1 => NextState}), 
    {ok, [Item]} = db_server:read(?TABLE_HARBOUR_REPORT_TASK, 
                   fun(X) -> X#?TABLE_HARBOUR_REPORT_TASK.id =:= Id end),
    %invariant
    case Item#?TABLE_HARBOUR_REPORT_TASK.state of 
        PrevState -> ok;
        _ -> throw(invalid_state_transition)
    end,
    Item1 = Item#?TABLE_HARBOUR_REPORT_TASK{state = NextState},
    ok = db_server:update([Item1]),
    {ok, Item1}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec(transition_next_item(PrevState :: harbour_report_task_state(), 
                    NextState :: harbour_report_task_state()) -> {ok, Item::harbour_report_task()} | {error, Reason :: term()}).
transition_next_item(PrevState, NextState) -> 
    ?LOG_INFO(#{service => work, what => transition_next_item, state0 => PrevState, state1 => NextState}), 
    case db_server:read(?TABLE_HARBOUR_REPORT_TASK, fun(X) -> X#?TABLE_HARBOUR_REPORT_TASK.state =:= PrevState end) of 
        {ok, [_|_] = Items} ->
                Item = lists:nth(1, lists:sort(Items)),
                Item1 = Item#?TABLE_HARBOUR_REPORT_TASK{state = NextState},
                ok = db_server:update([Item1]),
                {ok, Item1};
        {error, []} ->
                {error, no_items}
    end.


cron_loop(State) ->
    receive cancel -> ok
    after State#state.config#work_server_config.refresh ->
            ?LOG_INFO(#{service => work, what => cron_loop, state=>State}), 
            work_server:refresh()
    end.
