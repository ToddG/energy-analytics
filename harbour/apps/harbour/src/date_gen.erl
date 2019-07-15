%%%-------------------------------------------------------------------
%%% @author Todd Greenwood-Geer
%%% @copyright (C) 2019, Enviro Software Solutions, LLC
%%% @doc
%%%
%%% @end
%%% Created : 2019-07-04 18:48:29.855489
%%%-------------------------------------------------------------------
-module(date_gen).

-export([dates/2,
	 format/1,
         day_ranges/1
	]).

-type month()   :: 1..12.
-type day()     :: 1..31.
-type year()    :: 1..10000.
-type tdate()    :: {year(), month(), day()}.

%%%===================================================================
%%% Exported functions 
%%%===================================================================

%%%-------------------------------------------------------------------
%%% @doc
%%% Formatted date as YYYYMMDD
%%% @end
%%%-------------------------------------------------------------------
-spec(format(tdate()) -> string()).
format({Y, M, D}) ->
    Year = Y,
    Month = io_lib:format("~2..0B", [M]),
    Day = io_lib:format("~2..0B", [D]),
    lists:flatten(io_lib:format("~p~s~s", [Year, Month, Day])).

%%%-------------------------------------------------------------------
%%% @doc
%%% Given a list of dates [1,2,3,4] return a list of date tuples as
%%% [{1,2}, {2,3}, {3,4}]
%%% @end
%%%-------------------------------------------------------------------
day_ranges(L) -> day_ranges(L, []).

day_ranges([], Accum) -> Accum;
day_ranges([A,B], Accum) -> [{A, B} | Accum];
day_ranges([A,B|T], Accum) -> day_ranges([B|T], [{A, B} | Accum]).

%%%-------------------------------------------------------------------
%%% @doc
%%% Return the valid dates that are on or after Start and before
%%% or on End. 
%%%
%%% Example output:
%%%
%%%     {2019,6,23},
%%%     {2019,6,24},
%%%     {2019,6,25},
%%%     {2019,6,26},
%%%     {2019,6,27},
%%%     {2019,6,28},
%%%     {2019,6,29},
%%%     ...
%%%
%%% @end
%%%-------------------------------------------------------------------
-spec(dates(tdate(), tdate()) -> [tdate()]).
dates({StartYear, _, _} = Start, {EndYear, _, _} = End) ->
    filter(possible_dates(StartYear, EndYear), Start, End).

%%%===================================================================
%%% Internal functions
%%%===================================================================
possible_dates(StartYear, EndYear) ->
    [{Y, M, D} || Y <- lists:seq(StartYear, EndYear, 1), M <- lists:seq(1, 12), D <- lists:seq(1, 31)].

filter(Dates, {FirstYear, FirstMonth, FirstDay}, {LastYear, LastMonth, LastDay}) ->
    FFDfun = filter_first_dates_fun(FirstYear, FirstMonth, FirstDay),
    FLDfun = filter_last_dates_fun(LastYear, LastMonth, LastDay),
    D1 = lists:filter(fun ({Y, M, D}) -> calendar:valid_date(Y, M, D) end, Dates),
    D2 = lists:filter(fun ({Y, M, D}) -> FFDfun({Y, M, D}) end, D1),
    D3 = lists:filter(fun ({Y, M, D}) -> FLDfun({Y, M, D}) end, D2),
    D3.

filter_first_dates_fun(FirstYear, FirstMonth, FirstDay) ->
    FY = FirstYear,
    FM = FirstMonth,
    FD = FirstDay,
    fun ({Y, M, D}) -> filter_first_dates({Y, M, D}, FY, FM, FD) end.

filter_first_dates({Y, _, _}, FY, _, _) when Y < FY -> false;
filter_first_dates({Y, _, _}, FY, _, _) when Y > FY -> true;
filter_first_dates({Y, M, _}, FY, FM, _) when Y == FY, M < FM -> false;
filter_first_dates({Y, M, _}, FY, FM, _) when Y == FY, M > FM -> true;
filter_first_dates({Y, M, D}, FY, FM, FD) when Y == FY, M == FM, D < FD -> false;
filter_first_dates({Y, M, D}, FY, FM, FD) when Y == FY, M == FM, D >= FD -> true.

filter_last_dates_fun(LastYear, LastMonth, LastDay) ->
    LY = LastYear,
    LM = LastMonth,
    LD = LastDay,
    fun ({Y, M, D}) -> filter_last_dates({Y, M, D}, LY, LM, LD) end.

filter_last_dates({Y, _, _}, LY, _, _) when Y < LY -> true;
filter_last_dates({Y, _, _}, LY, _, _) when Y > LY -> false;
filter_last_dates({Y, M, _}, LY, LM, _) when Y == LY, M < LM -> true;
filter_last_dates({Y, M, _}, LY, LM, _) when Y == LY, M > LM -> false;
filter_last_dates({Y, M, D}, LY, LM, LD) when Y == LY, M == LM, D =< LD -> true;
filter_last_dates({Y, M, D}, LY, LM, LD) when Y == LY, M == LM, D > LD -> false.
