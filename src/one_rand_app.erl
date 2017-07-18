%%%-------------------------------------------------------------------
%% @doc one_rand public API
%% @end
%%%-------------------------------------------------------------------

-module(one_rand_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    case one_rand_sup:start_link() of
        {ok, _} = Res ->
            RandList = application:get_env(one_rand, rand_list, []),
            one_rand:start(RandList),
            Res;
        Err -> Err
    end.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
