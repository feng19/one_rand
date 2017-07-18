%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(or_ct_hook).

%% API
-export([
    init/2,
    terminate/1
]).


init(Id, _Opts) ->
    {ok, Started} = application:ensure_all_started(one_rand),
    true = lists:member(one_rand, Started),
    {ok, Id}.

terminate(_) ->
    ok = application:stop(one_rand),
    ok.