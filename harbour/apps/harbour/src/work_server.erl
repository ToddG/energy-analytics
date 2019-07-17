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

%% API
-export([start_link/0
         ,refresh/0
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
-record(work_server_config, {name       :: string(),
                             path       :: string(),
                             refresh    :: pos_integer()
                            }).
-type work_server_config()              :: #work_server_config{}.

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


refresh() ->
    gen_server:call(?MODULE, {refresh}).

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
    {ok, #state{config=ConfigRecord}}.

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
    refresh_reports(),
    {reply, ok, State};
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
    {noreply, State#state{config=ConfigRecord}};
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
%% @end
%%--------------------------------------------------------------------
-spec(parse_config([term()]) -> #work_server_config{}).
parse_config(ConfigList) ->
    Config      = proplists:get_value(work_server_config, ConfigList),
    DbName      = proplists:get_value(database_name, Config),
    DbPath      = proplists:get_value(database_path, Config),
    Refresh     = proplists:get_value(manifest_refresh_frequency_ms, Config),
    #work_server_config{name=DbName, path=DbPath, refresh=Refresh}.    


refresh_reports() ->
    {ok, ReportUrls} = manifest_server:reports(),
    {ok, Tasks} = db_server:read(task),
    DownloadTasks = lists:filter(fun(X) -> X#task.type == download_url end, Tasks),
    ExistingUrls = [X#task.data || X <- DownloadTasks],
    NewUrls = lists:subtract(ReportUrls, ExistingUrls),
    NewDownloadTasks = [#task{id=make_ref(), type=download_url, data=X} || X <- NewUrls],
    ok = db_server:create(NewDownloadTasks),
    ok.
