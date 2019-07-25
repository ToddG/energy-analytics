%%%-------------------------------------------------------------------
%%% @author Todd Greenwood-Geer
%%% @copyright (C) 2019, Enviro Software Solutions, LLC
%%% @doc
%%%
%%% @end
%%% Created : 2019-07-24 09:14:36.809881
%%%-------------------------------------------------------------------
-module(oasis_report_text_writer).
-include_lib("kernel/include/logger.hrl").
-include("records.hrl").


%% API
-export([serialize/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec(serialize(OasisReport :: oasis_report()) -> ok | {error, Reason :: term()}).
serialize(R) ->
    io:format("~p~n", [R]).
