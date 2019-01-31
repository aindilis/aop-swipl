%% :- use_module(library(predicate_streams)).

%% :- dynamic saved_output/1.

my_member(_,[]) :-
	fail.
my_member(A,[H|R]) :-
	A = H ;
	member(A,R).

should_be_wrapped(Goal) :-
	Goal =.. [P|R],
	(   P = ',' ->
	    maplist([A]>>(error([a,A]),aop_advice(A)),R) ; 
	    (
	     not(my_member(P,[module,dynamic,module,use_module,use_module,set_prolog_flag,member_,debug_print_hook,assertion_failed,pred_option,locate_clauses,pred_option,pred_option,pred_option,pop_compile_operators,push_compile_operators,push_compile_operators,quasi_quotation_syntax,alternate_syntax,quasi_quotation_syntax,xref_open_source,xref_close_source,xref_source_identifier,file_search_path,prolog_file_type,goal_expansion,prolog_predicate_name,prolog_clause_name,prolog_clause_name,with_output_to])),
	     not(my_member(P,[error,error_nl,wot,my_member,should_be_wrapped,do_aop_code_before,do_aop_code_after,goal_expansion,aop_advice])),
	     not(my_member(P,[nl,write_term]))
	    )).

wot(X,A) :-
	with_output_to(atom(A),write_term(X,[quoted(true)])).

error(Item) :-
	with_output_to(user_error,write_term(Item,[quoted(true)])).

error_nl :-
	with_output_to(user_error,nl).

aop_advice(Goal) :-
	do_aop_code_before(Goal),
	(   call(Goal) *-> Result = 'Exit' ; Result = 'Fail'  ),
	do_aop_code_after(Goal,Result),
	Result \== 'Fail'.

do_aop_code_before(Goal) :-
	wot(Goal,GoalAtom),
	atomic_list_concat(['   Call: (8) ',GoalAtom,' ?'],'',Output),
	error(Output), error_nl.

do_aop_code_after(Goal,Result) :-
	wot(Goal,GoalAtom),
	atomic_list_concat(['   ',Result,': (8) ',GoalAtom,' ?'],'',Output),
	error(Output), error_nl.

goal_expansion(Goal,aop_advice(Goal)) :-
	should_be_wrapped(Goal).

:- consult('test.pl').

%% trusted_redo_call_cleanup(Setup,Goal,Cleanup):- 
%% 	\+ \+ '$sig_atomic'(Setup),
%% 	catch( 
%% 	       (   (   Goal, deterministic(DET)),
%% 		   '$sig_atomic'(Cleanup),
%% 		   (   DET == true -> !
%% 	           ;   (   true;('$sig_atomic'(Setup),fail)))), 
%% 	       E, 
%% 	       (   '$sig_atomic'(Cleanup),throw(E))). 

% /home/andrewdo/lib/swipl/pack/predicate_streams/prolog/predicate_streams.pl

%% % the SETUPO is called at each REDO
%% % oh i can write out the aop_redo
%% aop_advice(Goal) :-
%% 	do_aop_code_before(Goal),
%% 	(   Goal *-> (Result = true; (aop_redo_code(Goal),fail) ) ; Result = fail ),
%% 	do_aop_code_after(Goal,Result),
%% 	Result \== fail.
