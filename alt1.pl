my_member(_,[]) :-
	fail.
my_member(A,[H|R]) :-
	A = H ;
	member(A,R).

should_be_wrapped(Goal) :-
	Goal =.. [P|_],
	%% write([p,P]),
	not(my_member(P,[module,use_module,use_module,set_prolog_flag,member_,debug_print_hook,assertion_failed,pred_option,locate_clauses,pred_option,pred_option,pred_option,pop_compile_operators,push_compile_operators,push_compile_operators,quasi_quotation_syntax,alternate_syntax,quasi_quotation_syntax,xref_open_source,xref_close_source,xref_source_identifier,file_search_path,prolog_file_type,goal_expansion,prolog_predicate_name,prolog_clause_name,prolog_clause_name])),
	not(my_member(P,[my_member,should_be_wrapped,do_aop_code_before,do_aop_code_after,goal_expansion,aop_advice])),
	not(my_member(P,[write,nl])).

aop_advice(Goal) :-
	do_aop_code_before(Goal),
	(   Goal -> Result = true ; Result = fail ),
	do_aop_code_after(Goal,Result).

do_aop_code_before(Goal) :-
	write('before goal: '),
	write(Goal),
	nl.

do_aop_code_after(Goal,Result) :-
	write('after goal: '),
	write(Goal),
	nl,
	write('result: '),
	write(Result),
	nl.

goal_expansion(Goal,aop_advice(Goal)) :-
	should_be_wrapped(Goal).

test :-
	write('hello'),
	nl.

