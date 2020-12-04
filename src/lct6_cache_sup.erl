%%%-------------------------------------------------------------------
%% @doc lct6_cache top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(lct6_cache_sup).

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
    Options = case application:get_env(lct6_cache, drop_interval) of
        undefined -> [];
        {ok, Value} when is_integer(Value) -> [{drop_interval, Value}];
        {ok, Value} -> [{drop_interval, list_to_integer(Value)}]
    end,
    SupFlags = #{strategy => one_for_all,
                 intensity => 1,
                 period => 5},
    ChildSpecs = [
        #{
            id => cache_server,
            start => {cache_server, start_link, [Options]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [cache_server]
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
