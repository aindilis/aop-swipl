my_member(_,[]) :-
	fail.
my_member(A,[H|R]) :-
	A = H ;
	member(A,R).

should_be_wrapped(Goal) :-
	Goal =.. [P|R],
	(   P = ',' ->
	    maplist([A]>>aop_advice(A),R) ; 
	    (
	     not(my_member(P,[module,use_module,use_module,set_prolog_flag,member_,debug_print_hook,assertion_failed,pred_option,locate_clauses,pred_option,pred_option,pred_option,pop_compile_operators,push_compile_operators,push_compile_operators,quasi_quotation_syntax,alternate_syntax,quasi_quotation_syntax,xref_open_source,xref_close_source,xref_source_identifier,file_search_path,prolog_file_type,goal_expansion,prolog_predicate_name,prolog_clause_name,prolog_clause_name,with_output_to])),
	     not(my_member(P,[my_member,should_be_wrapped,do_aop_code_before,do_aop_code_after,goal_expansion,aop_advice])),
	     not(my_member(P,[write,write_term,nl]))
	    )).

aop_advice(Goal) :-
	do_aop_code_before(Goal),
	with_output_to(atom(Output),Goal -> Result = true ; Result = fail),
	do_aop_code_after(Goal,Result,Output).
	%% (   Goal -> Result = true ; Result = fail),
	%% do_aop_code_after(Goal,Result).

do_aop_code_before(_) :-
	true.

do_aop_code_after(Goal,Result) :-
	write_term(hasResult(Goal,Result),[quoted(true)]),nl.

do_aop_code_after(Goal,Result,Output) :-
	write_term([goal(Goal),output(Output),hasResult(Result)],[quoted(true)]),write(','),nl.

goal_expansion(Goal,aop_advice(Goal)) :-
	should_be_wrapped(Goal).

test :-
	write('hello'),
	nl.

run :-
	test.


