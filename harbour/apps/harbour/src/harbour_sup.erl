%%%-------------------------------------------------------------------
%% @doc harbour top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(harbour_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [
                     {tag_pubsub,
                        {pubsub_server, start_link, []},
                        permanent,
                        10000,
                        worker,
                        [pubsub_server]}
                     ,{tag_config,
                        {config_server, start_link, []},
                        permanent,
                        10000,
                        worker,
                        [config_server]}
                     ,{tag_manifest,
                        {manifest_server, start_link, []},
                        permanent,
                        10000,
                        worker,
                        [manifest_server]}
                     ,{tag_work,
                        {work_server, start_link, []},
                        permanent,
                        10000,
                        worker,
                        [work_server]}
                     ,{tag_download,
                        {download_server, start_link, []},
                        permanent,
                        10000,
                        worker,
                        [download_server]}        
                 ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
%%
          
