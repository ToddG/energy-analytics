%%%-------------------------------------------------------------------
%%% @author Todd Greenwood-Geer
%%% @copyright (C) 2019, Enviro Software Solutions, LLC
%%% @doc
%%%
%%% @end
%%% Created : 2019-07-22 16:22:01.808103
%%%-------------------------------------------------------------------
-module(parse_xml).
-include_lib("kernel/include/logger.hrl").
-include("records.hrl").

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main([File|_]=Args) ->
    io:format("Args: ~p~n", [Args]),
    %oasis_report:parse_xml(File, fun sqlite3_handler/1),
    oasis_report:parse_xml(File, fun text_handler/1),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================
-spec(text_handler(OasisReport :: oasis_report()) -> ok | {error, Reason :: term()}).
text_handler(R) ->
    io:format("~p~n", [R]).

%-spec(sqlite3_handler(OasisReport :: oasis_report()) -> ok | {error, Reason :: term()}).
%sqlite3_handler(R) ->
%    ok.
%
%
%-spec(init_sqlite3_db(FQPath :: string(), TableInfo :: list()) -> ok | {error, Reason :: term()}).
%init_sqlite3_db(FQPath, TableInfo) ->
%    Dir = filename:dirname(FQPath),
%    %File = filename:basename(FQPath),
%    case filelib:is_regular(FQPath) of
%        true -> ok;
%        _ -> 
%            filelib:ensure_dir(Dir),
%            ok = sqlite3:open(sql3, [file, FQPath]),
%            TableInfo = [
%                         {id, integer, [{primary_key, [asc, autoincrement]}]},
%                         {timedate, string
%                         {name, text, [not_null, unique]},
%                         {age, integer, not_null},
%                         {wage, integer, []}],
%            sqlite3:create_table(sql3, oasis, TableInfo),
%            ok
%    end.
