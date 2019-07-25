%%%-------------------------------------------------------------------
%%% @author Todd Greenwood-Geer
%%% @copyright (C) 2019, Enviro Software Solutions, LLC
%%% @doc
%%%
%%% @end
%%% Created : 2019-07-24 08:46:25.791270
%%%-------------------------------------------------------------------
-module(oasis_report_sqlite3_writer).
-include_lib("kernel/include/logger.hrl").
-include("records.hrl").

%% API
-export([serialize_func/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec(serialize_func(FQPath :: string()) -> fun((OasisReport :: oasis_report()) -> ok | {error, Reason :: term()})).
serialize_func(F) ->
    fun(R) -> serialize(R, F) end.

-spec(serialize(OasisReport :: oasis_report(), FQPath :: string()) -> ok | {error, Reason :: term()}).
serialize(R, F) ->
    {ok, Db} = init_sqlite3_db(F),
    {ok, Statement} = esqlite3:prepare("
    INSERT INTO oasis (
                        timedate,            
                        timedate_posix,      
                        source,              
                        version,             
                        name,                
                        system,              
                        tz,                  
                        report,              
                        mkt_type,            
                        uom,                 
                        interval,            
                        sec_per_interval,    
                        data_item,           
                        resource_name,       
                        opr_date,            
                        opr_date_8601,       
                        interval_num,        
                        interval_start_gmt,  
                        interval_start_posix,
                        interval_end_gmt,    
                        interval_end_posix,  
                        value 
                      ) 
    VALUES (
                        ?1,
                        ?2,
                        ?3,
                        ?4,
                        ?5,
                        ?6,
                        ?7,
                        ?8,
                        ?9,
                        ?10,
                        ?11,
                        ?12,
                        ?13,
                        ?14,
                        ?15,
                        ?16,
                        ?17,
                        ?18,
                        ?19,
                        ?20,
                        ?21,
                        ?22
          );", Db),
    H = R#oasis_report.message_header,
    P = R#oasis_report.message_payload,
    N = P#message_payload.name,
    [write(H, N, I, Statement) || I <- P#message_payload.report_items],
    ok.

-spec(write(Header      :: message_header(),
            Name        :: string(),
            Item        :: report_item(),
            Statement   :: esqlite3:prepared_statement()
           ) -> ok | {error, Reason :: term()}).
write(H, N, I, Statement) ->
    IH = I#report_item.report_header,
    ID = I#report_item.report_data,
    ok = esqlite3:bind(Statement, [
                        H#message_header.timedate,
                        H#message_header.timedate_posix,
                        H#message_header.source,
                        H#message_header.version,
                        N,
                        IH#report_header.system,
                        IH#report_header.tz,
                        IH#report_header.report,
                        IH#report_header.mkt_type,
                        IH#report_header.uom,
                        IH#report_header.interval,
                        IH#report_header.sec_per_interval,
                        ID#report_data.data_item,
                        ID#report_data.resource_name,
                        ID#report_data.opr_date,
                        ID#report_data.opr_date_8601,
                        ID#report_data.interval_num,
                        ID#report_data.interval_start_gmt,
                        ID#report_data.interval_start_posix,
                        ID#report_data.interval_end_gmt,
                        ID#report_data.interval_end_posix,
                        ID#report_data.value
                                  ]),
    esqlite3:step(Statement),
    ok.



-spec(init_sqlite3_db(FQPath :: string()) -> {ok, esqlite3:connection()} | {error, Reason :: term()}).
init_sqlite3_db(FQPath) ->
    Dir = filename:dirname(FQPath),
    ?LOG_INFO(#{service=>parse, what=>init_sqlite3_db, db=>FQPath}), 
    case filelib:is_regular(FQPath) of
        true ->
            esqlite:open(FQPath);
        _ -> 
            io:format("Dir: ~p, FQPath: ~p~n", [Dir, FQPath]),
            ok = filelib:ensure_dir(Dir ++ "/"),
            {ok, Db} = esqlite3:open(FQPath),
            ok = esqlite3:exec("begin;", Db),
            ok = esqlite3:exec("
            CREATE TABLE IF NOT EXISTS
                        oasis ( id                       INTEGER PRIMARY KEY ASC AUTOINCREMENT,
                                timedate                 STRING,
                                timedate_posix           INTEGER,
                                source                   STRING,
                                version                  STRING,
                                name                     STRING,
                                system                   STRING,
                                tz                       STRING,
                                report                   STRING,
                                mkt_type                 STRING,
                                uom                      STRING,
                                interval                 INT,
                                sec_per_interval         INT,
                                data_item                STRING,
                                resource_name            STRING,
                                opr_date                 STRING,
                                opr_date_8601            STRING,
                                interval_num             INT,
                                interval_start_gmt       STRING,
                                interval_start_posix     INT,
                                interval_end_gmt         STRING,
                                interval_end_posix       INT,
                                value                    NUMBER);"
                               , Db),
            ok = esqlite3:exec("commit;", Db),
            {ok, Db}
    end.
