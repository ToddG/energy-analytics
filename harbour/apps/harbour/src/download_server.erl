%%%-------------------------------------------------------------------
%%% @author Todd Greenwood-Geer
%%% @copyright (C) 2019, Enviro Software Solutions, LLC
%%% @doc
%%%
%%% @end
%%% Created : 2019-07-17 07:54:43.797975
%%%-------------------------------------------------------------------
-module(download_server).

-behaviour(gen_server).
-include("../../common/include/tables.hrl").
-include_lib("kernel/include/logger.hrl").

%% API
-export([start_link/0
         ,download/0
         %,download/1 %% use a regex match
         ,download/2
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {
                    rate_limit      = 500                   :: pos_integer(),
                    download_path   = "/appdata/zip/"       :: string(),
                    refresh         = 3600000
         }).

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
%% Download all new files. Query the work_server for all remaining
%% stuff to download. This starts a sequential download that continues
%% for as long as there are items to download.
%% @end
%%--------------------------------------------------------------------
download() ->
    ?LOG_INFO(#{server=>download, what=>download}),
    gen_server:call(?MODULE, {download}). 

%%--------------------------------------------------------------------
%% @doc
%% Download url to file.
%% @end
%%--------------------------------------------------------------------
download(Url, File) ->
    ?LOG_INFO(#{server=>download, what=>download, url=>Url, file=>File}),
    gen_server:call(?MODULE, {download, Url, File}). 


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
    ConfigFile = filename:join(ConfigDir, "download.config"),
    {ok, RateLimit, DownloadPath, Refresh} = case file:consult(ConfigFile) of
        {ok, C} -> parse_config(C)
    end,
    State = #state{rate_limit=RateLimit, download_path=DownloadPath, refresh=Refresh},
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
handle_call({download}, _From, State) ->
    spawn_link(fun() -> sequential_download(State) end),
    {reply, ok, State};
handle_call({download, Url, File}, _From, State) ->
    ok = curl(Url, File, State#state.rate_limit),
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
handle_info({pubsub, {pubsub, {download_server_config, _} = Config}}, State) ->
    {ok, RateLimit, DownloadPath, Refresh} = parse_config([Config]),
    State1 = State#state{rate_limit=RateLimit, download_path=DownloadPath, refresh=Refresh},
    ?LOG_INFO(#{server=>download, what=>handle_info, pubsub=> download_server_config, state0=>State, state1=>State1}),
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
parse_config(C)->
    P               = proplists:get_value(download_server_config, C),
    RateLimit       = proplists:get_value(rate_limit, P),
    DownloadPath    = proplists:get_value(download_path, P),
    RefreshRate     = proplists:get_value(refresh, P),
    {ok, RateLimit, DownloadPath, RefreshRate}.


-spec(curl(Link :: string(), 
           OutputFile :: string(), 
           RateLimit :: pos_integer()) -> ok | {error, Reason :: term()}).
curl(Link, OutputFile, RateLimit)->
    ?LOG_INFO(#{server=>download, what=>curl, link=>Link, file=>OutputFile, rate=>RateLimit}),
    %Cmd = io_lib:format("curl --limit-rate ~pK -X GET -o ~s \"~s\"", [RateLimit, OutputFile, Link]), 
    %%Out = os:cmd(Cmd),
    %%?LOG_INFO(#{server=>download, what=>curl, link=>Link, file=>OutputFile, output=>Out}),
    ok.

-spec(sequential_download(State :: #state{}) -> ok | {error, Reason :: term()}).
sequential_download(State) ->
    case work_server:next_downloadable_item() of 
        {ok, T} -> 
            Id          = T#?TABLE_HARBOUR_REPORT_TASK.id,
            Url         = T#?TABLE_HARBOUR_REPORT_TASK.url,
            FileName    = T#?TABLE_HARBOUR_REPORT_TASK.filename,
            File        = filename:join(State#state.download_path, FileName),
            ok = curl(Url, File, State#state.rate_limit),
            work_server:item_download_complete(Id),
            download_server:download();
        _ -> ok
    end,
    ok.


cron_loop(State) ->
    receive cancel -> ok
    after State#state.refresh ->
            ?LOG_INFO(#{server=>download, what=>cron_loop, state=>State}),
            download_server:sequential_download(State)
    end.
