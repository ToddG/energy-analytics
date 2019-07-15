%%%-------------------------------------------------------------------
%%% @author Todd Greenwood-Geer
%%% @copyright (C) 2019, Enviro Software Solutions, LLC
%%% @doc
%%%
%%% @end
%%% Created : 2019-07-14 11:07:06.424357
%%%-------------------------------------------------------------------
-module(config_server).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% Inotify API
-export([
         inotify_event/3
        ]).
%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {}).
-include_lib("inotify/include/inotify.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    {ok, WatchDir} = application:get_env(harbour, watch_dir),
    Ref = inotify:watch(WatchDir),
    inotify:add_handler(Ref, ?MODULE, dir_change),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
        State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
        State :: #state{}) -> term()).
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
        Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%===================================================================
%%% inotify_evt callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% This module actually implements the {@link inotify_evt} behaviour, with a
%% function that simply prints the events to stdout.
%%
%% This function can be used as an example for how to implement a callback.
%%
%% @see print_events/1
%%
%% [INOTIFY] - dir_change #Ref<0.2388825619.1641545730.18801> - [open,isdir] 0 []
%% [INOTIFY] - dir_change #Ref<0.2388825619.1641545730.18801> - [access,isdir] 0 []
%% [INOTIFY] - dir_change #Ref<0.2388825619.1641545730.18801> - [access,isdir] 0 []
%% [INOTIFY] - dir_change #Ref<0.2388825619.1641545730.18801> - [close_nowrite,
%%                                                               isdir] 0 []
%% [INOTIFY] - dir_change #Ref<0.2388825619.1641545730.18801> - [open,isdir] 0 []
%% [INOTIFY] - dir_change #Ref<0.2388825619.1641545730.18801> - [access,isdir] 0 []
%% [INOTIFY] - dir_change #Ref<0.2388825619.1641545730.18801> - [close_nowrite,
%%                                                               isdir] 0 []
%% [INOTIFY] - dir_change #Ref<0.2388825619.1641545730.18801> - [delete] 0 "test02.confi"

%% @end
%%--------------------------------------------------------------------
inotify_event(_, Ref, ?inotify_msg(Masks, Cookie, OptionalName)) ->
    %%io:format("[INOTIFY] - ~p ~p - ~p ~p ~p~n", [Arg, Ref, Masks, Cookie, OptionalName]),
    {ok, WatchDir} = application:get_env(harbour, watch_dir),
    ChangedFile = filename:join(WatchDir, OptionalName),
    broadcast_file_change(Masks, ChangedFile),
    ok.


%%

%%--------------------------------------------------------------------
%% @doc
%%
%% This function responds to [close_write] events and publishes the 
%% list of tuples as events on the pubsub eventbus.
%%
%% [INOTIFY] - dir_change #Ref<0.1930962159.569638913.226951> - [close_write] 0 "bar.config"
%% @end
%%--------------------------------------------------------------------
broadcast_file_change(Masks, UpdatedConfigFile) when Masks == [?CLOSE_WRITE] ->
    case file:consult(UpdatedConfigFile) of 
        {ok, Terms} -> [broadcast_term(T) || T <- Terms];
        {error, Reason} -> io:format("Error: ~p~n", [Reason])
    end;
broadcast_file_change(Masks, UpdatedConfigFile) ->
    io:format("ChangeIgnored: ~p ~p~n", [Masks, UpdatedConfigFile]).

broadcast_term({Topic, Message}) ->
    pubsub_server:publish(Topic, Message).
