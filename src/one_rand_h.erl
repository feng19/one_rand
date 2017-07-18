%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(one_rand_h).
-include("ast_helper.hrl").

%% API
-export([
    create/4
]).

create(Module, Count, MaxMultiple, Tid) ->
    Forms = forms(Module, Count, MaxMultiple, Tid),
%%    io:format("~ts~n", [erl_prettypr:format(erl_syntax:form_list(Forms))]),
    compile_and_load_forms(Forms, [verbose, report_errors]).

forms(Module, Count, MaxMultiple, Tid) ->
    MaxCount = Count * MaxMultiple,
    Forms = [
        %% -module(Module).
        ?attribute(module, [?atom(Module)]),
        %% -export([rand/0]).
        ?attribute(export, [?list([
            ?arity_qualifier(rand, 0)
        ])]),
%%        rand() ->
%%            RandId = ets:update_counter(Tid, rand_id, {2, 1, 100000 * 3, 1}),
%%            Rand = ets:lookup_element(Tid, RandId, 2),
%%            case RandId rem 100000 of
%%                0 -> one_rand_srv:notice(rand);
%%                _ -> ok
%%            end,
%%            Rand.

        ?function(rand, [
            ?clause([], none, [
                ?match(?var('RandId'), ?apply(ets, update_counter, [?abstract(Tid), ?atom(rand_id),
                    ?tuple([?abstract(2), ?abstract(1), ?abstract(MaxCount), ?abstract(1)])])),
                ?match(?var('Rand'), ?apply(ets, lookup_element, [?abstract(Tid), ?var('RandId'), ?abstract(2)])),
                ?cases(?infix(?var('RandId'), 'rem', ?abstract(Count)), [
                    ?clause([?abstract(0)], none, [
                        ?apply(one_rand_srv, notice, [?abstract(Module)])
                    ]),
                    ?clause([?var('_')], none, [?abstract(ok)])
                ]),
                ?var('Rand')
            ])
        ])
    ],
    [erl_syntax:revert(Form) || Form <- Forms].

compile_and_load_forms(AbsCode, Opts) ->
    case compile:forms(AbsCode, Opts) of
        {ok, ModName, Binary} ->
            load_binary(ModName, Binary, Opts);
        {ok, ModName, Binary, _Warnings} ->
            load_binary(ModName, Binary, Opts)
    end.

load_binary(Name, Binary, Opts) ->
    code:purge(Name),
    File = beam_filename(Name, Opts),
    case code:load_binary(Name, File, Binary) of
        {module, Name} -> ok;
        {error, Reason} -> exit({error_loading_module, Name, Reason})
    end.

beam_filename(Mod, Opts) ->
    case lists:keyfind(outdir, 1, Opts) of
        {_, D} ->
            filename:join(D, atom_to_list(Mod) ++ code:objfile_extension());
        false ->
            ""
    end.