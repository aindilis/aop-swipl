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
	     not(my_member(P,[error,error_nl,wot,my_member,my_view,should_be_wrapped,do_aop_code_before,do_aop_code_after,goal_expansion,aop_advice])),
	     not(my_member(P,[write,write_term,nl]))
	    )).

wot(X,A) :-
	with_output_to(atom(A),write_term(X,[quoted(true)])).

error(Item) :-
	with_output_to(user_error,write_term(Item,[quoted(true)])).

error_nl :-
	with_output_to(user_error,nl).

aop_advice(Goal) :-
	do_aop_code_before(Goal),
	with_output_to(atom(Output),Goal -> Result = 'Exit' ; Result = 'Fail'),
	do_aop_code_after(Goal,Result,Output).

do_aop_code_before(Goal) :-
	wot(Goal,GoalAtom),
	atomic_list_concat(['   Call: (8) ',GoalAtom,' ?'],'',Output),
	error(Output), error_nl.

do_aop_code_after(Goal,Result) :-
	write_term(hasResult(Goal,Result),[quoted(true)]),nl.

do_aop_code_after(Goal,Result,Output1) :-
	wot(Goal,GoalAtom),
	atomic_list_concat(['   ',Result,': (8) ',GoalAtom,' ?'],'',Output),
	error(Output), error_nl.

goal_expansion(Goal,aop_advice(Goal)) :-
	should_be_wrapped(Goal).

:- consult('test.pl').