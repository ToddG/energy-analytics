-module(installer).

%% API exports
-export([main/1]).
-include_lib("../../harbour/apps/common/include/tables.hrl").

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    Path = "/appdata/db/mnesia",
    io:format("Installer is configuring target system... ~p~n", [Args]),
    %% directory
    application:set_env([{mnesia, [{dir, Path}]}]),
    E = application:get_all_env(mnesia),
    io:format("installer: ~p~n", [E]),
    %% schema
    DbNodes = [node()],
    case mnesia:create_schema(DbNodes) of
        ok -> 
            io:format("created db schema at: ~s~n", [Path]);
        {error,{Node,{already_exists, Node}}} ->
            io:format("db schema already exists at: ~s for node: ~s~n", [Path, Node])
    end,
    io:format("created schema on: ~p~n", [DbNodes]),
    %% tables
    application:start(mnesia),
    Tables = mnesia:system_info(tables),
    io:format("existing tables: ~p~n", [Tables]),
    TableSet = sets:from_list(Tables),
    case sets:is_element(task, TableSet) of
        true -> 
                io:format("table 'task' already exists: ~n", []);
        false ->
                mnesia:create_table(task, [
                                            {type, set},
                                            {attributes, record_info(fields, task)},
                                            {disc_copies, [node()]}
                                          ]),
                io:format("created 'task' table: ~n", [])
    end,
    case sets:is_element(rstate, TableSet) of
        true -> 
                io:format("table 'rstate' already exists: ~n", []);
        false ->
                mnesia:create_table(rstate, [
                                            {type, set},
                                            {attributes, record_info(fields, rstate)},
                                            {disc_copies, [node()]}
                                          ]),
                io:format("created 'rstate' table: ~n", [])
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
