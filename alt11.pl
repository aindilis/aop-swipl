:- dynamic test_data/3.
:- dynamic saved_output/1.
:- dynamic mode/0.

:- use_module('predicate_streams.pl').

my_mode(Mode) :-
	retractall(mode(_)),
	assert(mode(Mode)).
my_normal :-
	retractall(mode(_)),
	assert(mode(normal)).
my_trace :-
	retractall(mode(_)),
	assert(mode(trace)).
my_tests :-
	retractall(mode(_)),
	assert(mode(tests)).

:- assert(mode(normal)).

squelch(_) :-
	true.

my_member(_,[]) :-
	fail.
my_member(A,[H|R]) :-
	A = H ;
	my_member(A,R).

should_be_wrapped(Goal) :-
	Goal =.. [P|R],
	(   P = ',' ->
	    maplist([A]>>(error([a,A]),aop_advice(A)),R) ; 
	    (
	     not(my_member(P,[consult,module,use_module,set_prolog_flag,member_,debug_print_hook,assertion_failed,pred_option,locate_clauses,pred_option,pred_option,pred_option,pop_compile_operators,push_compile_operators,push_compile_operators,quasi_quotation_syntax,alternate_syntax,quasi_quotation_syntax,xref_open_source,xref_close_source,xref_source_identifier,file_search_path,prolog_file_type,goal_expansion,prolog_predicate_name,prolog_clause_name,prolog_clause_name,with_output_to,open])),
	     not(my_member(P,[error,error_nl,wot,my_member,should_be_wrapped,do_aop_code_before,do_aop_code_after,goal_expansion,aop_advice,write_data_to_file,writeq_data_to_file,generate_tests_for_goal])),
	     not(my_member(P,[nl,write_term,write_data_to_file,writeq_data_to_file]))
	    )).

wot(X,A) :-
	with_output_to(atom(A),write_term(X,[quoted(true)])).

errorq(Item) :-
	with_output_to(user_error,write_term(Item,[quoted(true)])).

error(Item,Options) :-
	with_output_to(user_error,write_term(Item,Options)).

error(Item) :-
	with_output_to(user_error,write_term(Item,[])).

error_nl :-
	with_output_to(user_error,nl).

aop_advice(Goal) :-
	do_aop_code_before(Goal),
	with_output_to_predicate([X]>>(
				       %% error([x,X]),
				       assert(saved_output(X))
				      ),
				 (   call(Goal) *-> (Result = true; (do_aop_code_redo(Goal),fail) ) ; Result = fail )),
	do_aop_code_after(Goal,Result),
	Result \== fail.

do_aop_code_before(Goal) :-
	(   mode(normal) -> true ;
	    (	mode(trace) ->
		(   
		    wot(Goal,GoalAtom),
		    atomic_list_concat(['   Call: (8) ',GoalAtom,' ?'],'',Output),
		    error(Output), error_nl
		) ;
		(   
		    true
		)
	    )).

do_aop_code_after(Goal,Result) :-
	(   mode(normal) -> true ;
	    (	mode(trace) ->
		(   
		    wot(Goal,GoalAtom),
		    atomic_list_concat(['   ',Result,': (8) ',GoalAtom,' ?'],'',Output),
		    error(Output), error_nl
		) ;
		(   
		    Goal =.. [P|A],
		    length(A,Arity),
		    atomic_list_concat([P,Arity],'__',Predicate),
		    (	Result \== fail -> TestGoal = Goal ; TestGoal = not(Goal)),
		    error(test(Predicate) :- TestGoal,[quoted(true)]),
		    error('.'), error_nl, error_nl,
		    (	(   predicate_property(P,[file(File)]),
			    assert(test_data(File,M:P/Arity,test(Predicate) :- TestGoal))) -> true ; true)
		)
	    )).

do_aop_code_redo(Goal) :-
	(   mode(normal) -> true ;
	    (	mode(trace) ->
		(   
		    wot(Goal,GoalAtom),
		    atomic_list_concat(['   Redo: (8) ',GoalAtom,' ?'],'',Output),
		    error(Output), error_nl
		) ;
		(   
		    true
		)
	    )).

write_data_to_file(Data,Filename) :-
	open(Filename, write, S),
	write(S,Data),
	close(S).

writeq_data_to_file(Data,Filename) :-
	open(Filename, write, S),
	writeq(S,Data),
	close(S).

generate_tests_for_goal(Goal) :-
	mode(Mode),
	my_mode(tests),
	call(Goal),
	%% findall(File,test_data(File,_,_),Files),
	%% findall([File,M:P],(member(File,Files),test_data(File,M:P,_)),MPs),
	%% findall([MP,Tests],(member(MP,MPs),findall(Test,(MP = [File,M:P],test_data(File,M:P,Test)),Tests)),MPTests),
	%% error(MPTests),
	my_mode(Mode).

goal_expansion(Goal,aop_advice(Goal)) :-
	should_be_wrapped(Goal).

:- consult('test.pl').


%% :- begin_tests(util2).

%% test(my_view_1):-my_view(b).

%% test(tsScry_1) :-
%% 	wot(tsScry(X = 1),A),
