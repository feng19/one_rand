%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(or_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile([export_all]).

all() ->
    [
        uniform_0,
        uniform_1
    ].

uniform_0(_) ->
    rand:seed(exs1024),
    Alg = rand:export_seed(),
    Count = 10000,
    {ok, _} = one_rand:start(or_0, Count, 5, Alg),
    [begin
         case or_0:uniform() == rand:uniform() of
             true -> ok
         end
     end || _ <- lists:seq(1, 6 * Count)],
    ok.

uniform_1(_) ->
    rand:seed(exs1024),
    Alg = rand:export_seed(),
    Count = 10000,
    {ok, _} = one_rand:start(or_1, Count, 5, Alg),
    [begin
         case or_1:uniform(1000) == rand:uniform(1000) of
             true -> ok
         end
     end || _ <- lists:seq(1, 6 * Count)],
    ok.