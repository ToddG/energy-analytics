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
main([Input,Output|_]=Args) ->
    io:format("Args: ~p~n", [Args]),
    io:format("input_xml:~p, output_db: ~p~n", [Input, Output]),
    F = oasis_report_sqlite3_writer:serialize_func(Output),
    oasis_report_xml_parser:parse_xml(Input, F),
    %oasis_report_xml_parser:parse_xml(Input, fun oasis_report_text_writer:serialize/1),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================
