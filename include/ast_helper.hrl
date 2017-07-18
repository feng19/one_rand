-ifndef(AST_HELPER_H).
-define(AST_HELPER_H, true).

-define(atom(Atom), erl_syntax:atom(Atom)).
-define(var(Var), erl_syntax:variable(Var)).
-define(underscore, erl_syntax:underscore()).

-define(apply(Fun, Args), erl_syntax:application(?atom(Fun), Args)).
-define(apply(Mod, Fun, Args), erl_syntax:application(?atom(Mod), ?atom(Fun), Args)).
-define(apply_(Fun, Args), erl_syntax:application(Fun, Args)).
-define(clause(Pattern, Guard, Body), erl_syntax:clause(Pattern, Guard, Body)).
-define(cases(Arg, Clauses), erl_syntax:case_expr(Arg, Clauses)).
-define(ifs(Clauses), erl_syntax:if_expr(Clauses)).
-define(record(Value, Name, Fields), erl_syntax:record_expr(Value, ?atom(Name), Fields)).
-define(record(Name, Fields), erl_syntax:record_expr(?atom(Name), Fields)).
-define(field(Name), erl_syntax:record_field(?atom(Name))).
-define(field(Name, Value), erl_syntax:record_field(?atom(Name), Value)).
-define(int(Val), erl_syntax:integer(Val)).
-define(tuple(Elems), erl_syntax:tuple(Elems)).
-define(function(Name, Clauses), erl_syntax:function(?atom(Name), Clauses)).
-define(match(Left, Right), erl_syntax:match_expr(Left, Right)).
-define(abstract(Term), erl_syntax:abstract(Term)).
-define(cons(Head, Tail), erl_syntax:cons(Head, Tail)).
-define(list(Elems), erl_syntax:list(Elems)).
-define(list(Elems, Tail), erl_syntax:list(Elems, Tail)).
-define(string(Str), erl_syntax:string(Str)).
-define(nil, erl_syntax:nil()).
-define(func(Module, Name, Arity), erl_syntax:implicit_fun(?atom(Module), ?atom(Name), ?int(Arity))).
-define(func(Name, Arity), erl_syntax:implicit_fun(?atom(Name), ?int(Arity))).
-define(func(Clauses), erl_syntax:fun_expr(Clauses)).
-define(access(Value, Record, Field), erl_syntax:record_access(Value, ?atom(Record), ?atom(Field))).
-define(infix(A, B, C), erl_syntax:infix_expr(A, erl_syntax:operator(B), C)).
-define(eq(A, B), ?infix(A, '==', B)).
-define(eeq(A, B), ?infix(A, '=:=', B)).
-define(neq(A, B), ?infix(A, '=/=', B)).
-define(gt(A, B), ?infix(A, '>', B)).
-define(gteq(A, B), ?infix(A, '>=', B)).
-define(lt(A, B), ?infix(A, '<', B)).
-define(lteq(A, B), ?infix(A, '=<', B)).
-define(AND(A, B), ?infix(A, 'and', B)).
-define(ANDALSO(A, B), ?infix(A, 'andalso', B)).
-define(OR(A, B), ?infix(A, 'or', B)).
-define(ORELSE(A, B), ?infix(A, 'orelse', B)).

-define(nif_element(N, VALUE), ?apply('element', [?int(N), VALUE])).
-define(nif_not(VALUE), ?apply('not', [VALUE])).
-define(nif_size(VALUE), ?apply('size', [VALUE])).
-define(nif_is_tuple(VALUE), ?apply('is_tuple', [VALUE])).
-define(nif_is_function(VALUE), ?apply('is_function', [VALUE])).

-define(list_comp(A, B), erl_syntax:list_comp(A, B)).
-define(generator(A, B), erl_syntax:generator(A, B)).

-define(arity_qualifier(Name, Arity), erl_syntax:arity_qualifier(?atom(Name), ?int(Arity))).
-define(attribute(Name, Values), erl_syntax:attribute(?atom(Name), Values)).
-define(export(Fun, Arity), ?attribute(export, [?list([?arity_qualifier(Fun, Arity)])])).
-define(export_all(List), ?attribute(export, [?list([?arity_qualifier(Fun, Arity) || {Fun, Arity} <- List])])).
-define(def_record(Name, Fields), ?attribute(record, [?atom(Name), ?tuple(Fields)])).

-define(ok(A), ?tuple([?atom(ok), A])).
-define(error(A), ?tuple([?atom(error), A])).
-define(error(A, B), ?error(?tuple([A, B]))).

-endif.