%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(one_rand).

%% API
-export([
    start/1,
    start/2,
    start/3,
    start/4
]).

start(RandList) ->
    [begin
         case size(Rand) of
             Size when Size >= 2 andalso Size =< 3 ->
                 {ok, _} = apply(?MODULE, start, tuple_to_list(Rand))
         end
     end || Rand <- RandList],
    ok.

start(Module, Count) ->
    start(Module, Count, 3, exs1024).
start(Module, Count, MaxMultiple) ->
    start(Module, Count, MaxMultiple, exs1024).
start(Module, Count, MaxMultiple, Alg) ->
    one_rand_sup:start_child([Module, Count, MaxMultiple, Alg]).