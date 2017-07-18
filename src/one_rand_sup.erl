%%%-------------------------------------------------------------------
%% @doc one_rand top level supervisor.
%% @end
%%%-------------------------------------------------------------------
-module(one_rand_sup).
-behaviour(supervisor).

%% API
-export([
    start_link/0,
    start_child/1
]).

%% Supervisor callbacks
-export([init/1]).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Args) ->
    supervisor:start_child(?MODULE, Args).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    SupFlags = #{strategy => simple_one_for_one, intensity => 5, period => 1},
    ChildSpec = #{
        id => one_rand_srv,
        start => {one_rand_srv, start_link, []},
        restart => temporary,
        shutdown => 5000,
        type => worker,
        modules => [one_rand_srv]
    },
    {ok, {SupFlags, [ChildSpec]}}.

%%====================================================================
%% Internal functions
%%====================================================================
