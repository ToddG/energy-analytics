%%%-------------------------------------------------------------------
%%% @author Todd Greenwood-Geer
%%% @copyright (C) 2019, Enviro Software Solutions, LLC
%%% @doc
%%%
%%% @end
%%% Created : 2019-07-22 16:22:01.808103
%%%-------------------------------------------------------------------
-module(parse_xml).

%% API exports
-export([main/1]).

-record(oasis_report, {     message_header              :: message_header(),
                            message_payload             :: message_payload()
                      }).
-record(message_header, {   timedate                    :: string(),
                            source                      :: string(),
                            version                     :: string()
                        }).
-record(message_payload, {  name                        :: string(),
                            report_items                :: [report_item()]
                         }).
-record(report_item, {      report_header               :: report_header(),
                            report_data                 :: report_data()
                     }).
-record(report_header, {    system                      :: string(),
                            tz                          :: string(),
                            report                      :: string(),
                            mkt_type                    :: string(),
                            uom                         :: string(),
                            interval                    :: string(),
                            sec_per_interval            :: string()
                       }).
-record(report_data, {      data_item                   :: string(),
                            resource_name               :: string(),
                            opr_date                    :: string(),
                            interval_num                :: pos_integer(),
                            interval_start_gmt          :: string(),
                            interval_end_gmt            :: string(),
                            value                       :: number()
                     }).

-type oasis_report()                                    :: #oasis_report{}.
-type message_header()                                  :: #message_header{}.
-type message_payload()                                 :: #message_payload{}.
-type report_item()                                     :: #report_item{}.
-type report_header()                                   :: #report_header{}.
-type report_data()                                     :: #report_data{}.

-record(state, {            path   = ""                 :: string(), 
                            accum  = #{}                :: map(),
                            oasis_report = #oasis_report{}    :: oasis_report()
               }).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main([File|_]=Args) ->
    io:format("Args: ~p~n", [Args]),
    foo(File),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
%callback(Event, Location, State) -> NewState


-spec(ender(LocalName :: string(), 
            Map :: map(), 
            Report :: oasis_report()) -> {map(), oasis_report()}).
ender("MessageHeader", M, R) ->
    TimeDate    = maps:get(["TimeDate",     "MessageHeader", "OASISReport"], M),
    Source      = maps:get(["Source",       "MessageHeader", "OASISReport"], M),
    Version     = maps:get(["Version",      "MessageHeader", "OASISReport"], M),
    {M, R#oasis_report{message_header=#message_header{
                                         timedate  = TimeDate,
                                         source    = Source,
                                         version   = Version}}};
ender("MessagePayload", M, R) ->
    Name        = maps:get(["name",         "RTO", "MessagePayload", "OASISReport"], M),
    {M, R#oasis_report{message_payload=#message_payload{
                                          name  = Name}}};
ender("REPORT_ITEM", M, #oasis_report{message_payload = 
                                          #message_payload{report_items = ReportItems}} = R) ->
    ReportHeader= maps:get(["REPORT_HEADER",    "REPORT_ITEM", "RTO", "MessagePayload", "OASISReport"], M),
    ReportData  = maps:get(["REPORT_DATA",      "REPORT_ITEM", "RTO", "MessagePayload", "OASISReport"], M),
    {M, R#oasis_report{message_payload=#message_payload{
                                          report_items = [#report_item{
                                                             report_header=ReportHeader, 
                                                             report_data=ReportData} | ReportItems]}}};
ender("REPORT_HEADER", M, R) ->
    SYSTEM      = maps:get(["SYSTEM",       "REPORT_HEADER", "REPORT_ITEM", "RTO", "MessagePayload", "OASISReport"], M),
    TZ          = maps:get(["TZ",           "REPORT_HEADER", "REPORT_ITEM", "RTO", "MessagePayload", "OASISReport"], M),
    REPORT      = maps:get(["REPORT",       "REPORT_HEADER", "REPORT_ITEM", "RTO", "MessagePayload", "OASISReport"], M),
    MKT_TYPE    = maps:get(["MKT_TYPE",     "REPORT_HEADER", "REPORT_ITEM", "RTO", "MessagePayload", "OASISReport"], M),
    UOM         = maps:get(["UOM",          "REPORT_HEADER", "REPORT_ITEM", "RTO", "MessagePayload", "OASISReport"], M),
    INTERVAL    = maps:get(["INTERVAL",     "REPORT_HEADER", "REPORT_ITEM", "RTO", "MessagePayload", "OASISReport"], M),
    SEC_PER_INTERVAL = maps:get(["SEC_PER_INTERVAL", "REPORT_HEADER", "REPORT_ITEM", "RTO", "MessagePayload", "OASISReport"], M),
    M1 = M#{"REPORT_HEADER" => #report_header{
                                                system             = SYSTEM,
                                                tz                 = TZ,
                                                report             = REPORT,
                                                mkt_type           = MKT_TYPE,
                                                uom                = UOM,
                                                interval           = INTERVAL,
                                                sec_per_interval   = SEC_PER_INTERVAL}},
    {M1, R};
ender("REPORT_DATA", M, R) ->
    DATA_ITEM       = maps:get(["DATA_ITEM",        "REPORT_DATA", "REPORT_ITEM", "RTO", "MessagePayload", "OASISReport"], M),
    RESOURCE_NAME   = maps:get(["RESOURCE_NAME",    "REPORT_DATA", "REPORT_ITEM", "RTO", "MessagePayload", "OASISReport"], M),
    OPR_DATE        = maps:get(["OPR_DATE",         "REPORT_DATA", "REPORT_ITEM", "RTO", "MessagePayload", "OASISReport"], M),
    INTERVAL_NUM    = maps:get(["INTERVAL_NUM",     "REPORT_DATA", "REPORT_ITEM", "RTO", "MessagePayload", "OASISReport"], M),
    INTERVAL_START_GMT = maps:get(["INTERVAL_START_GMT", "REPORT_DATA", "REPORT_ITEM", "RTO", "MessagePayload", "OASISReport"], M),
    INTERVAL_END_GMT = maps:get(["INTERVAL_END_GMT", "REPORT_DATA", "REPORT_ITEM", "RTO", "MessagePayload", "OASISReport"], M),
    VALUE           = maps:get(["VALUE",            "REPORT_DATA", "REPORT_ITEM", "RTO", "MessagePayload", "OASISReport"], M),
    M1 = M#{"REPORT_DATA" => #report_data{
                                            data_item          = DATA_ITEM         ,
                                            resource_name      = RESOURCE_NAME     ,
                                            opr_date           = OPR_DATE          ,
                                            interval_num       = INTERVAL_NUM      ,
                                            interval_start_gmt = INTERVAL_START_GMT,
                                            interval_end_gmt   = INTERVAL_END_GMT  ,
                                            value              = VALUE}},
    {M1, R}.


%callback(endDocument, _Location, #state{report=R}) ->
callback(endDocument, _Location, State) ->
    R = State#state.oasis_report,
    io:format("endDocument: ~p~n", [State]),
    io:format("header: ~p~n", [R#oasis_report.message_header]),
    P = R#oasis_report.message_payload,
    I = P#message_payload.report_items,
    io:format("record count: ~p~n", [length(I)]),
    State;
callback(startDocument, _Location, _State) ->
    io:format("startDocument~n", []),
    #state{};
callback({characters, Data}, _Location, #state{path=Path, accum=M} = State) ->
    State#state{accum=M#{Path => Data}};
callback({startElement, _, LocalName, _, _}, _Location, #state{path=Path, accum=M} = State) ->
    NewPath = [LocalName | Path],
    State#state{path = NewPath, accum=M#{NewPath => undefined}};
callback({endElement, _Uri, LocalName, _QualifiedName}, _Location, #state{path=Path, accum=M, oasis_report=R} = State) ->
    {ok, Value} = maps:find(Path, M),
    io:format("~p : ~p~n", [Path, Value]),
%    {M1, R1} = ender(LocalName, M, R),
    M2 = M#{Path => undefined},
    [_|P1] = Path,
    State#state{path=P1, accum=M2, oasis_report=R};
callback(Event, _Location, State) ->
    io:format("event: ~p~n", [Event]),
    State.

foo(F) ->
    xmerl_sax_parser:file(F, [{event_fun, fun callback/3}]).
