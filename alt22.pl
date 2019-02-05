:- dynamic goal_expansion/1.

:- dynamic test_data/3.
:- dynamic saved_output/1.
:- dynamic autotest_tests/1.
:- dynamic mode/0.

:- use_module('predicate_streams.pl').
:- use_module(library(regex)).

string_match_p(String,Regex) :-
	regex(Regex, [], String, []).

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

should_be_wrapped(Predicate,R) :-
	(   Predicate = ',' ->
	    true ; %% maplist([A]>>(aop_advice(A)),R) ; 
	    (	
		not(my_member(Predicate,[:-,=..,!,in,consult,module,use_module,module_file,call,include,load_files,set_prolog_flag,member_,debug_print_hook,assertion_failed,pred_option,locate_clauses,pred_option,pred_option,pred_option,pop_compile_operators,push_compile_operators,push_compile_operators,quasi_quotation_syntax,alternate_syntax,quasi_quotation_syntax,xref_open_source,xref_close_source,xref_source_identifier,file_search_path,prolog_file_type,goal_expansion,prolog_predicate_name,prolog_clause_name,prolog_clause_name,with_output_to,open,listing,is])),
		not(my_member(Predicate,[error,error_nl,wot,should_be_wrapped,do_aop_code_before,do_aop_code_after,goal_expansion,aop_advice,write_data_to_file,writeq_data_to_file,generate_tests_for_goal])),
		not(my_member(Predicate,[nl,write,write_term,write_data_to_file,writeq_data_to_file,string_match_p])),
		not(my_member(Predicate,[repeatingTimer,resource_manager_load_state])),
		length(R,Arity),
		(   
		    (	
			M:Predicate/Arity = M:F/A, functor(P,F,A), source_file(M:P,File)
		    ) ;   
		    (	
			Predicate/Arity = F/A, functor(P,F,A),
			predicate_property(M:P,file(File)),
			\+ predicate_property(M:P,imported_from(_))
		    )
		),
		string_match_p(File,'^/var/lib/myfrdcsa/.*\\.pl$')
	    )).

%% should_be_wrapped(Goal) :-
%% 	Goal =.. [P|R],
%% 	(   P = ',' ->
%% 	    maplist([A]>>(error([a,A]),aop_advice(A)),R) ; 
%% 	    (
%% 	     not(my_member(P,[consult,module,use_module,set_prolog_flag,member_,debug_print_hook,assertion_failed,pred_option,locate_clauses,pred_option,pred_option,pred_option,pop_compile_operators,push_compile_operators,push_compile_operators,quasi_quotation_syntax,alternate_syntax,quasi_quotation_syntax,xref_open_source,xref_close_source,xref_source_identifier,file_search_path,prolog_file_type,goal_expansion,prolog_predicate_name,prolog_clause_name,prolog_clause_name,with_output_to,open,listing,is,!])),
%% 	     not(my_member(P,[error,error_nl,wot,my_member,should_be_wrapped,do_aop_code_before,do_aop_code_after,goal_expansion,aop_advice,write_data_to_file,writeq_data_to_file,generate_tests_for_goal])),
%% 	     not(my_member(P,[nl,write,write_term,write_data_to_file,writeq_data_to_file,string_match_p]))
%% 	    )).

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
	(   
	    mode(normal) -> (   call(Goal) *-> (Result = true; (do_aop_code_redo(Goal),fail) ) ; Result = fail ) ;
	    (	
		with_output_to_predicate([X]>>(
					       asserta(saved_output(X))
					      ),
					 (   call(Goal) *-> (Result = true; (do_aop_code_redo(Goal),fail) ) ; Result = fail )),
		findall(X,saved_output(X),Xs),
		retractall(saved_output(_)),
		atomic_list_concat(Xs,'',SavedOutput)
	    %% error([savedOutput,SavedOutput]),error_nl,
	    )
	),
	do_aop_code_after(Goal,Result,SavedOutput),
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

do_aop_code_after(Goal,Result,SavedOutput) :-
	(   mode(normal) -> true ;
	    (	mode(trace) ->
		(   
		    wot(Goal,GoalAtom),
		    atomic_list_concat(['   ',Result,': (8) ',GoalAtom,' ?'],'',Output),
		    error(Output), error_nl
		) ;
		(   
		    Goal =.. [Pred|Args],
		    length(Args,Arity),
		    atomic_list_concat([Pred,Arity],'__',Predicate),
		    error(test(Predicate) :- tester(Goal,Result,SavedOutput),[quoted(true)]),
		    error('.'), error_nl, error_nl,
		    (	(   
			    (	
				%% M = user,
				(   
				    M:Pred/Arity = M:F/A, functor(P,F,A), source_file(M:P,File),
				    %% error([File,M:Pred/A]),error_nl,
				    assert(test_data(File,M:Pred/A,test(Predicate) :- tester(Goal,Result,SavedOutput)))
				) ;   
				(   
				    Pred/Arity = F/A, functor(P,F,A),
				    predicate_property(M:P,file(File)),
				    \+ predicate_property(M:P,imported_from(_)),
				    %% error([File,M:Pred/A]),error_nl,
				    assert(test_data(File,M:Pred/A,test(Predicate) :- tester(Goal,Result,SavedOutput)))
				)
			    )
			) -> true ; true)
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

append_data_to_file(Data,Filename) :-
	open(Filename, append, S),
	write(S,Data),
	close(S).

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
	setof(File,A^B^test_data(File,A,B),Files),
	member(File,Files),
	string_match_p(File,'^/var/lib/myfrdcsa/.*\\.pl$'),
	error([file,File]),
	atomic_list_concat([File,'t'],'',OutputFile),
	error_nl,
	wotp(generate_for_file(File),Atom),
	error([atom,Atom]),error_nl,
	write_data_to_file(Atom,OutputFile),
	assert(autotest_tests(File)),
	error('Wrote to file: '),
	error(OutputFile),
	error_nl,
	fail.
generate_tests_for_goal(Goal) :-
	true.

generate_for_file(File) :-
	write_term(:- begin_tests(File),[quoted(true)]),
	write('.'), nl, nl,
	setof(MPA,A^test_data(File,MPA,A),MPAs),
	member(MPA,MPAs),
	error([mpa,MPA]),error_nl,
	generate_for_mpa(File,MPA),
	fail.
generate_for_file(File) :-
	write_term(:- end_tests(File),[quoted(true)]),
	write('.'), nl,
	true.

generate_for_mpa(File,MPA) :-
	write('%% '),
	write_term(MPA,[quoted(true)]),
	nl,nl,
	setof(Test,test_data(File,MPA,Test),Tests),
	member(Test,Tests),
	error([test,Test]),error_nl,
	generate_for_test(File,MPA,Test).

generate_for_test(File,MPA,Test) :-
	write_term(Test,[quoted(true)]),
	write('.'), nl, nl.
generate_for_test(File,MPA,Test) :-
	true.

wotp(Goal,Atom) :-
	(   with_output_to_predicate([X]>>(assert(saved_output(X))),call(Goal)) -> true ; true),
	findall(X,saved_output(X),Xs),
	atomic_list_concat(Xs,'',Atom),
	%% saved_output(Atom),
	retractall(saved_output(_)).

write_autotest_file :-
	findall(File,autotest_tests(File),Files),
	wotp(write_autotest_tests(Files),Atom),
	atomic_list_concat([
			    '\'Line 1 of\' ... \'autotest.tst\'<>nl.\n\n',
			    Atom,
			    '\n\' autotest.tst\'<>nl.'
			   ],'',Output),
	%% error(Output),error_nl,
	write_data_to_file(Output,'/var/lib/myfrdcsa/codebases/minor/flp-tests/autotest.tst'),
	error_nl,error('Wrote to file: /var/lib/myfrdcsa/codebases/minor/flp-tests/autotest.tst'),error_nl.

write_autotest_tests(Files) :-
	member(File,Files),
	write('run_tests('),
	write_term(File,[quoted(true)]),
	write(') :: s.'),
	nl,
	fail.
write_autotest_tests :-
	true.

write_consult_file :-
	findall(File,autotest_tests(File),Files),
	wotp(write_consult_consults(Files),Atom),
	write_data_to_file(Atom,'/var/lib/myfrdcsa/codebases/minor/flp-tests/autotest.pl'),
	error_nl,error('Wrote to file: /var/lib/myfrdcsa/codebases/minor/flp-tests/autotest.pl'),error_nl.

write_consult_consults(Files) :-
	member(File,Files),
	atomic_list_concat([File,'t'],'',OutputFile),
	write(':- consult('),
	write_term(OutputFile,[quoted(true)]),
	write(').'),
	nl,
	fail.
write_consult_consults :-
	true.

%% term_expansion(Term,aop_advice(Term2)) :-
%% 	Term =.. [P|R],
%% 	Term2 =.. [P|R],
%% 	should_be_wrapped(P,R).

%% goal_expansion(Goal,aop_advice(Goal2)) :-
%% 	Goal =.. [P|R],
%% 	Goal2 =.. [P|R],
%% 	should_be_wrapped(P,R).

:- consult('lib.pl').

do :-
	generate_tests_for_goal(factorial(5,X)),
	generate_tests_for_goal(run2),
	generate_tests_for_goal(generatePageFor(resourceManager,[desiredAcquisitions,DesiredAquisitions])),
	write_autotest_file,
	write_consult_file.

goal_expansion(Goal,aop_advice(Goal)) :-
	Goal =.. [P|R],
	(   P = ',' ->
	    (
	     member(R1,R),
	     R1 =.. [P2|R2],
	     should_be_wrapped(P2,R2),
	     Goal2 =.. [P2|R2],
	     goal_expansion(Goal2,aop_advice(Goal2)),
	     %% error_nl,
	     fail
	    ) ;
	    should_be_wrapped(P,R)).

:- consult('test.pl').
:- consult('t/factorial.pl').
