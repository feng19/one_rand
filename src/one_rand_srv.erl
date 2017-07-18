%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(one_rand_srv).
-behaviour(gen_server).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% API
-export([
    start_link/4,
    notice/1
]).

-record(state, {module, count, max_count, max_multiple, tid, alg, id}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
start_link(Module, Count, MaxMultiple, Alg) when is_atom(Module) andalso is_integer(Count)
    andalso is_integer(MaxMultiple) andalso MaxMultiple >= 3 andalso is_atom(Alg) ->
    gen_server:start_link({local, Module}, ?MODULE, [Module, Count, MaxMultiple, Alg], []).

notice(Module) ->
    gen_server:cast(Module, notice).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Module, Count, MaxMultiple, Alg]) ->
    Id = 2 * Count,
    Tid = ets:new(one_rand, [public, set]),
    ok = one_rand_h:create(Module, Count, MaxMultiple, Tid),
    ets:insert(Tid, {rand_id, 1}),
    rand:seed(Alg),
    [ets:insert(Tid, {N, rand:uniform()}) || N <- lists:seq(1, Id)],
    {ok, #state{module = Module, count = Count, max_count = Count * MaxMultiple,
        max_multiple = MaxMultiple, alg = Alg, tid = Tid, id = Id}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(notice, State) ->
    Id = State#state.id,
    Tid = State#state.tid,
    Count = State#state.count,
    NewId =
        case (Id + Count) == State#state.max_count of
            true ->
                [ets:insert(Tid, {N, rand:uniform()}) || N <- lists:seq(Id + 1, State#state.max_count)],
                0;
            _ ->
                NewIdTemp = Id + State#state.count,
                [ets:insert(Tid, {N, rand:uniform()}) || N <- lists:seq(Id + 1, NewIdTemp)],
                NewIdTemp
        end,
    {noreply, State#state{id = NewId}};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
