-module(installer).

%% API exports
-export([main/1]).
-include_lib("../../harbour/apps/common/include/tables.hrl").

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main([Path|_]) ->
    MnesiaPath  = filename:join([Path, "db", "mnesia"]),
    ZipPath     = filename:join([Path, "zip"]),
    XmlPath     = filename:join([Path, "xml"]),
    LogPath     = filename:join([Path, "log"]),
    ok = filelib:ensure_dir(io_lib:format("~s/", [MnesiaPath])),
    ok = filelib:ensure_dir(io_lib:format("~s/", [ZipPath])),
    ok = filelib:ensure_dir(io_lib:format("~s/", [XmlPath])),
    ok = filelib:ensure_dir(io_lib:format("~s/", [LogPath])),
    io:format("Installing system to... ~p~n", [Path]),
    io:format(" > db dir    : ~p~n", [MnesiaPath]),
    io:format(" > zip dir   : ~p~n", [ZipPath]),
    io:format(" > xml dir   : ~p~n", [XmlPath]),
    io:format(" > log dir   : ~p~n", [LogPath]),
    %% directory
    application:set_env([{mnesia, [{dir, MnesiaPath}]}]),
    E = application:get_all_env(mnesia),
    io:format("installer: ~p~n", [E]),
    %% schema
    DbNodes = [node()],
    case mnesia:create_schema(DbNodes) of
        ok -> 
            io:format("created db schema at: ~s~n", [MnesiaPath]);
        {error,{Node,{already_exists, Node}}} ->
            io:format("db schema already exists at: ~s for node: ~s~n", [MnesiaPath, Node])
    end,
    io:format("created schema on: ~p~n", [DbNodes]),
    %% tables
    application:start(mnesia),
    Tables = mnesia:system_info(tables),
    io:format("existing tables: ~p~n", [Tables]),
    TableSet = sets:from_list(Tables),
    case sets:is_element(?TABLE_HARBOUR_REPORT_TASK, TableSet) of
        true -> 
                io:format("table '~p' already exists: ~n", [?TABLE_HARBOUR_REPORT_TASK]);
        false ->
                mnesia:create_table(?TABLE_HARBOUR_REPORT_TASK, [
                                            {type, set},
                                            {attributes, record_info(fields, ?TABLE_HARBOUR_REPORT_TASK)},
                                            {disc_copies, [node()]}
                                          ]),
                io:format("created '~p' table: ~n", [?TABLE_HARBOUR_REPORT_TASK])
    end,
    case sets:is_element(?TABLE_HARBOUR_RECORD_STATE, TableSet) of
        true -> 
                io:format("table '~p' already exists: ~n", [?TABLE_HARBOUR_RECORD_STATE]);
        false ->
                mnesia:create_table(?TABLE_HARBOUR_RECORD_STATE, [
                                            {type, set},
                                            {attributes, record_info(fields, ?TABLE_HARBOUR_RECORD_STATE)},
                                            {disc_copies, [node()]}
                                          ]),
                io:format("created '~p' table: ~n", [?TABLE_HARBOUR_RECORD_STATE])
    end,
    
    %% done
    mnesia:stop(),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================
%create_table({Table, Record}, ExistingTables) ->
%    case sets:is_element(Table, ExistingTables) of
%        true -> 
%                io:format("table already exists: ~p~n", [Table]);
%        false ->
%                mnesia:create_table(Table, [{attributes, record_info(fields, Record)}]),
%                io:format("created table: ~p~n", [Table])
%    end.
%
