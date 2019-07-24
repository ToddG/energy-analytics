%%%-------------------------------------------------------------------
%%% @author Todd Greenwood-Geer
%%% @copyright (C) 2019, Enviro Software Solutions, LLC
%%% @doc
%%%
%%% @end
%%% Created : 2019-07-22 16:22:01.808103
%%%-------------------------------------------------------------------
-module(oasis_report).
-include_lib("kernel/include/logger.hrl").

%% API exports
-export([parse_xml/2]).

-record(oasis_report, {     message_header                  :: message_header(),
                            message_payload                 :: message_payload()
                      }).
-record(message_header, {   timedate                        :: string(),
                            source                          :: string(),
                            version                         :: string()
                        }).
-record(message_payload, {  name                            :: string(),
                            report_items                    :: [report_item()]
                         }).
-record(report_item, {      report_header                   :: report_header(),
                            report_data                     :: report_data()
                     }).
-record(report_header, {    system                          :: string(),
                            tz                              :: string(),
                            report                          :: string(),
                            mkt_type                        :: string(),
                            uom                             :: string(),
                            interval                        :: string(),
                            sec_per_interval                :: string()
                       }).
-record(report_data, {      data_item                       :: string(),
                            resource_name                   :: string(),
                            opr_date                        :: string(),
                            interval_num                    :: pos_integer(),
                            interval_start_gmt              :: string(),
                            interval_end_gmt                :: string(),
                            value                           :: number()
                     }).

-type oasis_report()                                        :: #oasis_report{}.
-type message_header()                                      :: #message_header{}.
-type message_payload()                                     :: #message_payload{}.
-type report_item()                                         :: #report_item{}.
-type report_header()                                       :: #report_header{}.
-type report_data()                                         :: #report_data{}.

-record(state, {            path   = ""                     :: string(), 
                            accum  = #{}                    :: map(),
                            oasis_report = oasis_report()   :: oasis_report()
               }).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec(parse_xml(File :: string(), 
		Handler :: fun((oasis_report()) -> ok | {error, Reason :: term()}) ) 
      -> ok | error).
parse_xml(F, H) ->
    {ok, #state{oasis_report=R}, _} = xmerl_sax_parser:file(F, [{event_fun, fun callback/3}]),
    ?LOG_INFO(#{service=>oasis_report, what=>handler, file=>F}), 
    H(R).



%%====================================================================
%% Internal functions
%%====================================================================

oasis_report() ->
    #oasis_report{
       message_header = #message_header{},
       message_payload = #message_payload{
                           name         = undefined,
                           report_items = []
                           }
      }.

push(K, V, M) ->
    M1 = M#{K => V},
    M1.

pop(K, M) ->
    case maps:is_key(K, M) of
        true ->
            V = maps:get(K, M),
            M1 = maps:remove(K, M),
            {M1, V};
        _ -> 
            io:format("could not find: ~p~n", [K]),
            {M, error}
    end.

-spec(ender(LocalName :: string(),
            Path :: list(),
            Map :: map(), 
            Report :: oasis_report()) -> {map(), oasis_report()}).
ender("MessageHeader", P, M, R) ->
    {M1,TimeDate}           = pop(["TimeDate",     "MessageHeader", "OASISReport"], M),
    {M2, Source}            = pop(["Source",       "MessageHeader", "OASISReport"], M1),
    {M3, Version}           = pop(["Version",      "MessageHeader", "OASISReport"], M2),
    MH = R#oasis_report.message_header,
    MH1 = MH#message_header{
                            timedate   = TimeDate, 
                            source     = Source, 
                            version    = Version},
    R1 = R#oasis_report{message_header=MH1},
    {M4, _} = pop(P, M3),
    {M4, R1};
ender("MessagePayload", P, M, R) ->
    {M1, Name}              = pop(["name",         "RTO", "MessagePayload", "OASISReport"], M),
    MP  = R#oasis_report.message_payload,
    MP1 = MP#message_payload{name=Name},
    R1  = R#oasis_report{message_payload=MP1},
    M2  = pop(P, M1),
    {M2, R1};
ender("REPORT_ITEM", P, M, R) ->
    MP = R#oasis_report.message_payload,
    RI = MP#message_payload.report_items,
    {M1, ReportHeader}      = pop(["REPORT_HEADER",     "REPORT_ITEM", "RTO", "MessagePayload", "OASISReport"], M),
    {M2, ReportData}        = pop(["REPORT_DATA",       "REPORT_ITEM", "RTO", "MessagePayload", "OASISReport"], M1),
    NewReportItem = #report_item{report_header=ReportHeader, report_data=ReportData},
    NRI = [NewReportItem | RI],
    R1 = R#oasis_report{message_payload = MP#message_payload{report_items=NRI}},
    {M3, _} = pop(P, M2),
    {M3, R1};
ender("REPORT_HEADER", P, M, R) ->
    {M1, SYSTEM}            = pop(["SYSTEM",       "REPORT_HEADER", "REPORT_ITEM", "RTO", "MessagePayload", "OASISReport"], M),
    {M2, TZ}                = pop(["TZ",           "REPORT_HEADER", "REPORT_ITEM", "RTO", "MessagePayload", "OASISReport"], M1),
    {M3, REPORT}            = pop(["REPORT",       "REPORT_HEADER", "REPORT_ITEM", "RTO", "MessagePayload", "OASISReport"], M2),
    {M4, MKT_TYPE}          = pop(["MKT_TYPE",     "REPORT_HEADER", "REPORT_ITEM", "RTO", "MessagePayload", "OASISReport"], M3),
    {M5, UOM}               = pop(["UOM",          "REPORT_HEADER", "REPORT_ITEM", "RTO", "MessagePayload", "OASISReport"], M4),
    {M6, INTERVAL}          = pop(["INTERVAL",     "REPORT_HEADER", "REPORT_ITEM", "RTO", "MessagePayload", "OASISReport"], M5),
    {M7, SEC_PER_INTERVAL}  = pop(["SEC_PER_INTERVAL", "REPORT_HEADER", "REPORT_ITEM", "RTO", "MessagePayload", "OASISReport"], M6),
    M8 = M7#{P => #report_header{
                                                system             = SYSTEM,
                                                tz                 = TZ,
                                                report             = REPORT,
                                                mkt_type           = MKT_TYPE,
                                                uom                = UOM,
                                                interval           = INTERVAL,
                                                sec_per_interval   = SEC_PER_INTERVAL}},
    %%this is popped by REPORT_ITEM
    {M8, R};
ender("REPORT_DATA", P, M, R) ->
    {M1, DATA_ITEM}         = pop(["DATA_ITEM",             "REPORT_DATA", "REPORT_ITEM", "RTO", "MessagePayload", "OASISReport"], M),
    {M2, RESOURCE_NAME}     = pop(["RESOURCE_NAME",         "REPORT_DATA", "REPORT_ITEM", "RTO", "MessagePayload", "OASISReport"], M1),
    {M3, OPR_DATE}          = pop(["OPR_DATE",              "REPORT_DATA", "REPORT_ITEM", "RTO", "MessagePayload", "OASISReport"], M2),
    {M4, INTERVAL_NUM}      = pop(["INTERVAL_NUM",          "REPORT_DATA", "REPORT_ITEM", "RTO", "MessagePayload", "OASISReport"], M3),
    {M5, INTERVAL_START_GMT}= pop(["INTERVAL_START_GMT", "REPORT_DATA", "REPORT_ITEM", "RTO", "MessagePayload", "OASISReport"], M4),
    {M6, INTERVAL_END_GMT}  = pop(["INTERVAL_END_GMT",   "REPORT_DATA", "REPORT_ITEM", "RTO", "MessagePayload", "OASISReport"], M5),
    {M7, VALUE}             = pop(["VALUE",                 "REPORT_DATA", "REPORT_ITEM", "RTO", "MessagePayload", "OASISReport"], M6),
    M8 = M7#{P => #report_data{
                                            data_item          = DATA_ITEM         ,
                                            resource_name      = RESOURCE_NAME     ,
                                            opr_date           = OPR_DATE          ,
                                            interval_num       = INTERVAL_NUM      ,
                                            interval_start_gmt = INTERVAL_START_GMT,
                                            interval_end_gmt   = INTERVAL_END_GMT  ,
                                            value              = VALUE}},
    %%this is popped by REPORT_ITEM
    {M8, R};
ender(_, _P, M, R) ->
    {M, R}.


callback(endDocument, _Location, State) ->
    R = State#state.oasis_report,
    P = R#oasis_report.message_payload,
    Items = P#message_payload.report_items,
    ?LOG_INFO(#{service => oasis_report, what => endDocument, report=>P#message_payload.name, count=>length(Items)}), 
    State;
callback(startDocument, _Location, _State) ->
    #state{};
callback({characters, Data}, _Location, #state{path=Path, accum=M} = State) ->
    M1 = push(Path, Data, M),
    S1 = State#state{accum=M1},
    S1;
callback({startElement, _, LocalName, _, _}, _Location, #state{path=Path, accum=M} = State) ->
    NewPath = [LocalName | Path],
    M1 = push(NewPath, undefined, M),
    S1 = State#state{path = NewPath, accum=M1},
    S1;
callback({endElement, _Uri, LocalName, _QualifiedName}, _Location, #state{path=Path, accum=M, oasis_report=R} = State) ->
    ?LOG_DEBUG(#{service=>oasis_report, what=>endElement, path=>Path}), 
    {M1, R1} = ender(LocalName, Path, M, R),
    [_|P1] = Path,
    State#state{path=P1, accum=M1, oasis_report=R1};
callback(_Event, _Location, State) ->
    State.
