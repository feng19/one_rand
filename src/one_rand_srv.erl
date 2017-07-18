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
    create_next/1
]).

-record(state, {module, count, max_count, max_multiple, tid, alg, id, next_fun, r}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Module, Count, MaxMultiple, Alg) when is_atom(Module) andalso is_integer(Count) andalso Count >= 10000
    andalso is_integer(MaxMultiple) andalso MaxMultiple >= 3 ->
    gen_server:start_link({local, Module}, ?MODULE, [Module, Count, MaxMultiple, Alg], []).

create_next(Module) ->
    gen_server:cast(Module, create_next).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Module, Count, MaxMultiple, Alg0]) ->
    {#{type := Alg, next := NextFun}, R0} = rand:seed_s(Alg0),
    Id = 2 * Count,
    Tid = ets:new(one_rand, [public, set]),
    ok = one_rand_h:create(Module, Count, MaxMultiple, Alg, Tid),
    ets:insert(Tid, {rand_id, 0}),
    R = insert_seq(1, Id, Tid, NextFun, R0),
    {ok, #state{module = Module, count = Count, max_count = Count * MaxMultiple,
        max_multiple = MaxMultiple, alg = Alg, tid = Tid, id = Id, next_fun = NextFun, r = R}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(create_next, State) ->
    Id = State#state.id,
    Tid = State#state.tid,
    Count = State#state.count,
    {NewId, NewR} =
        case (Id + Count) == State#state.max_count of
            true ->
                R = insert_seq(Id + 1, State#state.max_count, Tid, State#state.next_fun, State#state.r),
                {0, R};
            _ ->
                NewIdTemp = Id + State#state.count,
                R = insert_seq(Id + 1, NewIdTemp, Tid, State#state.next_fun, State#state.r),
                {NewIdTemp, R}
        end,
    {noreply, State#state{id = NewId, r = NewR}};
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

insert_seq(End, End, Tid, NextFun, R) ->
    {V, R1} = NextFun(R),
    ets:insert(Tid, {End, V}),
    R1;
insert_seq(Start, End, Tid, NextFun, R) ->
    {V, R1} = NextFun(R),
    ets:insert(Tid, {Start, V}),
    insert_seq(Start + 1, End, Tid, NextFun, R1).