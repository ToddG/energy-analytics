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
-record(state, {            path = ""                       :: string(), 
                            temp                            :: string(),
                            report                          :: oasis_report()
               }).
-record(oasis_report, {name, foo}).

%-type state()                                               :: #state{}.
-type oasis_report()                                        :: #oasis_report{}.

callback(startDocument, _Location, _State) ->
    #state{};
callback({characters, Data}, _Location, State) ->
    State#state{temp=Data};
callback({startElement, _, LocalName, _, _}, _Location, #state{path=Path} = State) ->
    State#state{path = [LocalName | Path]};
callback({endElement, _Uri, _LocalName, _QualifiedName}, _Location, #state{path=Path, temp=Value} = State) ->
    io:format("path: ~p, value: ~p~n", [Path, Value]),
    [_|PrevPath] = Path,
    State#state{path = PrevPath};
callback(_Event, _Location, State) ->
    State.

foo(F) ->
    xmerl_sax_parser:file(F, [{event_fun, fun callback/3}]).
