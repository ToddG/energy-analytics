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
-record(state, {            path        = ""            :: string(), 
                            accum       = #{}           :: map(),
                            report                      :: oasis_report()
               }).
-record(oasis_report, {     name, 
                            foo
                      }).

-type oasis_report()                                    :: #oasis_report{}.

callback(startDocument, _Location, _State) ->
    io:format("startDocument~n", []),
    #state{};
callback({characters, Data}, _Location, #state{path=Path, accum=M} = State) ->
    %io:format("characters: ~p~n", [Data]),
    State#state{accum=M#{Path => Data}};
callback({startElement, _, LocalName, _, _}, _Location, #state{path=Path, accum=M} = State) ->
    NewPath = [LocalName | Path],
    %io:format("startElement: ~p~n", [NewPath]),
    State#state{path = NewPath, accum=M#{NewPath => undefined}};
callback({endElement, _Uri, _LocalName, _QualifiedName}, _Location, #state{path=Path, accum=M} = State) ->
    io:format("~p : ~p~n", [Path, maps:find(Path, M)]),
    M1 = M#{Path => undefined},
    [_|P1] = Path,
    State#state{path = P1, accum=M1};
callback(Event, _Location, State) ->
    %io:format("event: ~p~n", [Event]),
    State.

foo(F) ->
    xmerl_sax_parser:file(F, [{event_fun, fun callback/3}]).
