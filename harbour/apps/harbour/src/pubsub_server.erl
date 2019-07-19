%%%-------------------------------------------------------------------
%%% @author Todd Greenwood-Geer
%%% @copyright (C) 2019, Enviro Software Solutions, LLC
%%% @doc
%%%
%%% @end
%%% Created : 2019-07-14 02:08:43.831261
%%%-------------------------------------------------------------------
-module(pubsub_server).

-behaviour(gen_server).
-include_lib("kernel/include/logger.hrl").

%% API
-export([start_link/0]).
-export([publish/2,
         subscribe/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {topics = #{} :: map()}).
-record(pub, {topic :: term() , message :: term()}).
-record(sub, {topic, subscriber :: subscriber()}).
%-type pub() :: #pub{}.
%-type sub() :: #sub{}.
-type subscriber() :: pid() | term().


%%%===================================================================
%%% API
%%%===================================================================
%%% TODO: remove subsribers if/when their process dies? Perhaps just 
%%% TODO: do this for Pids and not for registered names...
%%%
-spec(publish(term(), term()) -> ok | {error, Reason :: term()}).
publish(Topic, Message) ->
    ?LOG_INFO(#{service => pubsub, what => publish, topic => Topic, message => Message}), 
    gen_server:call(?MODULE, {publish, #pub{topic=Topic, message=Message}}). 

-spec(subscribe(term(), term()) -> ok | {error, Reason :: term()}).
subscribe(Topic, Subscriber) ->
    ?LOG_INFO(#{service => pubsub, what => subscribe, topic => Topic, subscriber => Subscriber}), 
    gen_server:call(?MODULE, {subscribe, #sub{topic=Topic, subscriber=Subscriber}}). 


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
handle_call({subscribe, #sub{topic=Topic, subscriber=Subscriber}}, _From, State) ->
    TM = State#state.topics,
    case maps:is_key(Topic, TM) of
        true -> SubscriberList0 = maps:get(Topic, TM),
                SubscriberList1 = [Subscriber | SubscriberList0],
                TM1 = TM#{Topic := SubscriberList1},
                State1 = State#state{topics=TM1},
                {reply, ok, State1};
        _ ->    TM1 = TM#{Topic => [Subscriber]},
                State1 = State#state{topics=TM1},
                {reply, ok, State1}
    end;
handle_call({publish, #pub{topic=Topic, message=Message}}, _From, State) ->
    TM = State#state.topics,
    case maps:is_key(Topic, TM) of
        true -> [Pid ! {pubsub, {Topic, Message}} || Pid <- maps:get(Topic, TM)],
                {reply, ok, State};
        _ ->    {reply, {error, topic_not_found}, State} 
    end;
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
