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

%% API
-export([start_link/0,
         reports/0,
         reports/2
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {reports}).

-type month()   :: 1..12.
-type day()     :: 1..31.
-type year()    :: 1..10000.
-type tdate()    :: {year(), month(), day()}.

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
-spec(reports() -> {ok, [term()]} | {error, Reason :: term()}).
reports() -> 
    gen_server:call(?MODULE, {reports}). 

%%--------------------------------------------------------------------
%% @doc
%% Generate list of report urls, between start and end dates (close close).
%% @end
%%--------------------------------------------------------------------
-spec(reports(tdate(), tdate()) -> {ok, [term()]} | {error, Reason :: term()}).
reports(StartDate, EndDate) -> 
    gen_server:call(?MODULE, {reports_start_end, StartDate, EndDate}). 


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
    ok = pubsub_server:subscribe(manifest_server_oasis_reports, manifest_server),
    {ok, WatchDir} = application:get_env(harbour, watch_dir),
    ConfigFile = filename:join(WatchDir, "manifest.config"),
    {Reports} = case file:consult(ConfigFile) of
        {ok, Terms} -> 
            MSOR = lists:last(lists:filter(fun({K,_}) -> K == manifest_server_oasis_reports end, Terms)),
            {MSOR}
    end,
    {ok, #state{reports = parse_reports(Reports)}}.


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
handle_call({reports}, _From, State) ->
    {manifest_server_oasis_reports, P} = State#state.reports,
    StartDate   = proplists:get_value(start_date, P),
    {EndDate, _} = calendar:local_time(),
    {reply, common_reports(P, StartDate, EndDate), State};
handle_call({reports_start_end, StartDate, EndDate}, _From, State) ->
    {manifest_server_oasis_reports, P} = State#state.reports,
    {reply, common_reports(P, StartDate, EndDate), State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

common_reports(P, StartDate, EndDate) ->
    Website     = proplists:get_value(website, P),
    Context     = proplists:get_value(context, P),
    Reports     = proplists:get_value(reports, P),
    Dates       = date_gen:dates(StartDate, EndDate),
    Ranges      = date_gen:day_ranges(Dates),
    Reports     = proplists:get_value(reports, P),
    ListSZ = [single_zip_url(R, date_gen:format(Start), date_gen:format(End), Website, Context) || #{type := single_zip} = R <- Reports, {Start, End} <- Ranges],
    {ok, ListSZ}.


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
handle_info({manifest_server_oasis_reports, Reports}, State) ->
    {noreply, State#state{reports = parse_reports(Reports)}};
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

parse_reports(R) ->
    %% TODO: parse the reports to guarantee they are correct
    R.


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
single_zip_url(#{type := single_zip, name := Name, mri := MRI, version := Version, params := Params}, Start, End, Website, Context) ->
    single_zip_url(Name, Start, End, MRI, Version, Params, Website, Context).

single_zip_url(OASISReport, StartDateTime, EndDateTime, MarketRunID, Version, Params, Website, Context) -> 
	ReportType = "SingleZip",
	io_lib:format("http://~s/~s/~s?queryname=~s&startdatetime=~sT07:00-0000&enddatetime=~sT07:00-0000&market_run_id=~s&version=~p&~s", 
		      [Website, Context, ReportType, OASISReport, StartDateTime, EndDateTime, MarketRunID, Version, Params]).

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
group_zip_url(OASISReport, StartDateTime, EndDateTime, Version) -> 
    	WEBSite = "oasis.caiso.com",
	Context = "oasisapi",
	ReportType = "GroupZip",
	io_lib:format("http://~s/~s/~s?groupid=~s&startdatetime=~p&enddatetime=~p&Version=~p", 
		      [WEBSite, Context, ReportType, OASISReport, StartDateTime, EndDateTime, Version]).

file(Path, OASISReport, StartDateTime, EndDateTime, Version) -> 
	io_lib:format("~s/~s_~s_~s_~p.zip", [Path, OASISReport, StartDateTime, EndDateTime, Version]).

file(Path, OASISReport, StartDateTime, EndDateTime, MarketRunId, Version) -> 
	io_lib:format("~s/~s_~s_~s_~s_~p.zip", [Path, OASISReport, StartDateTime, EndDateTime, MarketRunId, Version]).
