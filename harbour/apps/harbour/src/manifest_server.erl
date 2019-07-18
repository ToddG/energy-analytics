%%%-------------------------------------------------------------------
%%% @author Todd Greenwood-Geer
%%% @copyright (C) 2019, Enviro Software Solutions, LLC
%%% @doc
%%%
%%% @end
%%% Created : 2019-07-14 13:20:45.641914
%%%-------------------------------------------------------------------
-module(manifest_server).

-behaviour(gen_server).
-include("../../common/include/dates.hrl").

%% API
-export([start_link/0
         ,manifests/0
         ,manifests/2
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE). 

-record(state,              {oasis_config   :: oasis_config(),
                             oasis_reports  :: oasis_reports() 
                            }).

-record(oasis_config,       {website        :: string(),
                             context        :: string(),
                             start_date     :: tdate()
                            }).

-record(oasis_reports,      {reports        :: [map()]}).

-type oasis_config()                        :: #oasis_config{}.
-type oasis_reports()                       :: #oasis_reports{}.

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
%% Generate list of report urls.
%% @end
%%--------------------------------------------------------------------
-spec(manifests() -> {ok, [term()]} | {error, Reason :: term()}).
manifests() -> 
    gen_server:call(?MODULE, {manifests}). 

%%--------------------------------------------------------------------
%% @doc
%% Generate list of report urls, between start and end dates (close close).
%% @end
%%--------------------------------------------------------------------
-spec(manifests(tdate(), tdate()) -> {ok, [term()]} | {error, Reason :: term()}).
manifests(StartDate, EndDate) -> 
    gen_server:call(?MODULE, {manifests_start_end, StartDate, EndDate}). 


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
    ok = pubsub_server:subscribe(manifest_server_config, manifest_server),
    ok = pubsub_server:subscribe(oasis_reports_config, manifest_server),
    {ok, ConfigDir} = application:get_env(harbour, config_dir),
    ManifestConfigFile = filename:join(ConfigDir, "manifest.config"),
    {ok, OasisServerConfig} = case file:consult(ManifestConfigFile) of
        {ok, C1} -> parse_oasis_server_config(C1)
    end,
    OasisReportConfigFile = filename:join(ConfigDir, "oasis_reports.config"),
    {ok, OasisReportsConfig} = case file:consult(OasisReportConfigFile) of
        {ok, C2} -> parse_oasis_report_config(C2)
    end,
    {ok, #state{oasis_config = OasisServerConfig, oasis_reports = OasisReportsConfig}}.

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
handle_call({manifests}, _From, State) ->
    C = State#state.oasis_config,
    R = State#state.oasis_reports,
    StartDate = C#oasis_config.start_date,
    {EndDate, _} = calendar:local_time(),
    {ok, Reports} = oasis_reports(C, R, StartDate, EndDate),
    {reply, {ok, [{oasis, Reports}]}, State};
handle_call({manifests_start_end, StartDate, EndDate}, _From, State) ->
    C = State#state.oasis_config,
    R = State#state.oasis_reports,
    {ok, Reports} =  oasis_reports(C, R, StartDate, EndDate),
    {reply, {ok, [{oasis, Reports}]}, State};
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
handle_info({pubsub, {oasis_reports_config, _} = Config}, State) ->
    {ok, ReportsConfig} = parse_oasis_report_config([Config]),
    {noreply, State#state{oasis_reports = ReportsConfig}};
handle_info({pubsub, {manifest_server_config, _} = Config}, State) ->
    {ok, OasisConfig} = parse_oasis_server_config([Config]),
    {noreply, State#state{oasis_config = OasisConfig}};
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
parse_oasis_server_config(P0) ->
    P1          = proplists:get_value(manifest_server_config, P0),
    P2          = proplists:get_value(oasis,        P1),
    Website     = proplists:get_value(website,      P2),
    Context     = proplists:get_value(context,      P2),
    StartDate   = proplists:get_value(start_date,   P2),
    {ok, #oasis_config{website=Website, context=Context, start_date=StartDate}}.

parse_oasis_report_config(P0) ->
    P1          = proplists:get_value(oasis_reports_config, P0),
    Reports     = proplists:get_value(reports, P1),
    {ok, #oasis_reports{reports=Reports}}.

%%%-------------------------------------------------------------------
%%% @doc
%%% Construct OASIS single report url per docs:
%%% 
%%% URL?queryname=<A>&startdatetime=<D>&enddatetime=<D>&market_run_id=<A>&version=<A>&varParameters
%%% 
%%% Where:
%%% 	URL = http://oasiswebsite/context-path/SingleZip
%%% 	context-path = oasisapi
%%% 	For Production : <oasiswebsite> is oasis.caiso.com
%%% 	For MAPStage : <oasiswebsite> is oasis.caiso.com
%%%
%%% Mandatory Parameters:
%%%	startdatetime = valid operating start datetime in GMT (yyyymmddThh24:miZ)
%%%	enddatetime = valid operating end datetime in GMT (yyyymmddThh24:miZ)
%%%		which is equal or greater than <startdate>
%%%	queryname = valid reportname,
%%%	refer to the XML Query Name in the document
%%%	market_run_id = valid market type
%%%	version = API version (1 for the GMT 2013 release)
%%%
%%% Variable Parameters:
%%%	varParameters
%%%	variable Parameters are defined for each Report and its specific Filter options
%%% @end
%%%-------------------------------------------------------------------
single_zip(#{type := single_zip, name := Name, mri := MRI, version := Version, params := Params}, Start, End, Website, Context) ->
    {single_zip_url(Name, Start, End, MRI, Version, Params, Website, Context), single_zip_filename(Name, Start, End, MRI, Version, Params, Website, Context)}.

single_zip_url(OASISReport, StartDateTime, EndDateTime, MarketRunID, Version, Params, Website, Context) -> 
	ReportType = "SingleZip",
	io_lib:format("http://~s/~s/~s?queryname=~s&startdatetime=~sT07:00-0000&enddatetime=~sT07:00-0000&market_run_id=~s&version=~p&~s", 
		      [Website, Context, ReportType, OASISReport, StartDateTime, EndDateTime, MarketRunID, Version, Params]).


single_zip_filename(OASISReport, StartDateTime, EndDateTime, MarketRunID, Version, Params, Website, Context) -> 
	io_lib:format("~s:sz:~s:~s:~s:~s:~s:~p:~s.zip", 
		      [OASISReport, Website, Context, StartDateTime, EndDateTime, MarketRunID, Version, Params]).
    
    

%%%-------------------------------------------------------------------
%%% @doc
%%% Construct OASIS report url per docs:
%%%
%%% URL?groupid=<A>&startdatetime=<D>&enddatetime=<D>&version=<A>
%%% Where:
%%% 	URL = http://<oasiswebsite>/oasisapi/GroupZip
%%% 	context-path = oasisapi
%%% 	For Production : <oasiswebsite> is oasis.caiso.com
%%% 	For MAPStage : <oasiswebsite> is oasismap.caiso.com
%%% Mandatory Parameters:
%%% 	Groupid = valid groupid
%%% 	startdatetime = valid operating start datetime with timezone offset(yyyymmddThh24:miZ)
%%% 	enddatetime = valid operating end datetime with timezone offset (yyyymmddThh24:miZ) 
%%% 		(Only applicable for HASP,RTM groups)
%%% 	version = API version (1 for the GMT 2013 release)
%%% @end
%%%-------------------------------------------------------------------
%%% group_zip_url(OASISReport, StartDateTime, EndDateTime, Version) -> 
%%%     	WEBSite = "oasis.caiso.com",
%%% 	Context = "oasisapi",
%%% 	ReportType = "GroupZip",
%%% 	io_lib:format("http://~s/~s/~s?groupid=~s&startdatetime=~p&enddatetime=~p&Version=~p", 
%%% 		      [WEBSite, Context, ReportType, OASISReport, StartDateTime, EndDateTime, Version]).
%%% 
%%% file(Path, OASISReport, StartDateTime, EndDateTime, Version) -> 
%%% 	io_lib:format("~s/~s_~s_~s_~p.zip", [Path, OASISReport, StartDateTime, EndDateTime, Version]).
%%% 
%%% file(Path, OASISReport, StartDateTime, EndDateTime, MarketRunId, Version) -> 
%%% 	io_lib:format("~s/~s_~s_~s_~s_~p.zip", [Path, OASISReport, StartDateTime, EndDateTime, MarketRunId, Version]).
%%%
%%%

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec(oasis_reports(C::oasis_config(), 
                    R::oasis_reports(), 
                    StartDate :: tdate(), 
                    EndDate :: tdate()) -> {ok, [{string(), string()}]}).
oasis_reports(C, R, StartDate, EndDate) ->
    Website     = C#oasis_config.website,
    Context     = C#oasis_config.context,
    Reports     = R#oasis_reports.reports,
    Dates       = date_gen:dates(StartDate, EndDate),
    Ranges      = date_gen:day_ranges(Dates),
    SingleZipsList = [single_zip(X, date_gen:format(Start), date_gen:format(End), Website, Context) || 
              #{type := single_zip} = X <- Reports, {Start, End} <- Ranges],
    {ok, SingleZipsList}.

