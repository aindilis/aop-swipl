:- dynamic test_data/3.
:- dynamic saved_output_1/1.
:- dynamic saved_output_2/1.
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
	     not(my_member(P,[consult,module,use_module,set_prolog_flag,member_,debug_print_hook,assertion_failed,pred_option,locate_clauses,pred_option,pred_option,pred_option,pop_compile_operators,push_compile_operators,push_compile_operators,quasi_quotation_syntax,alternate_syntax,quasi_quotation_syntax,xref_open_source,xref_close_source,xref_source_identifier,file_search_path,prolog_file_type,goal_expansion,prolog_predicate_name,prolog_clause_name,prolog_clause_name,with_output_to,open,listing])),
	     not(my_member(P,[error,error_nl,wot,my_member,should_be_wrapped,do_aop_code_before,do_aop_code_after,goal_expansion,aop_advice,write_data_to_file,writeq_data_to_file,generate_tests_for_goal])),
	     not(my_member(P,[nl,write_term,write_data_to_file,writeq_data_to_file,string_match_p]))
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
				       assert(saved_output_1(X))
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
		    Goal =.. [Pred|Args],
		    length(Args,Arity),
		    atomic_list_concat([Pred,Arity],'__',Predicate),
		    (	Result \== fail -> TestGoal = Goal ; TestGoal = not(Goal)),
		    error(test(Predicate) :- TestGoal,[quoted(true)]),
		    error('.'), error_nl, error_nl,
		    (	(   
			    (	
				%% M = user,
				(   
				    M:Pred/Arity = M:F/A, functor(P,F,A), source_file(M:P,File),
				    %% error([File,M:Pred/A]),error_nl,
				    assert(test_data(File,M:Pred/A,test(Predicate) :- TestGoal))
				) ;   
				(   
				    Pred/Arity = F/A, functor(P,F,A),
				    predicate_property(M:P,file(File)),
				    \+ predicate_property(M:P,imported_from(_)),
				    %% error([File,M:Pred/A]),error_nl,
				    assert(test_data(File,M:Pred/A,test(Predicate) :- TestGoal))
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

	%% findall([M:P/Arity],test_data(M:P/Arity,_),MPs),
	%% findall([MP,Tests],(member(MP,MPs),findall(Test,(MP = [M:P/Arity],test_data(M:P/Arity,Test)),Tests)),MPTests),

	setof(File,A^B^test_data(File,A,B),Files),
	%% error([files,Files]),error_nl,

	findall([File,M,P,A],(member(File,Files),test_data(File,M:P/A,_)),TmpMPAs),
	setof(MPA,member(MPA,TmpMPAs),MPAs),
	%% error([mpas,MPAs]),error_nl,
	
	findall([MPA,Tests],(member(MPA,MPAs),findall(Test,(MPA = [File,M,P,A],test_data(File,M:P/A,Test)),Tests)),AllMPATests),
	error([allMPATests,AllMPATests]),error_nl,

	generate_tests_files(AllMPATests),
	my_mode(Mode).

generate_tests_files(AllMPATests) :-
	member(MPATests,AllMPATests),
	MPATests = [[File,M,P,A],Tests],
	string_match_p(File,'^/var/lib/myfrdcsa/.*\\.pl$'),
	atomic_list_concat([File,'t'],'',OutputFile),
	error_nl,
	wotp(generate_tests_file(MPATests),Atom),
	write_data_to_file(Atom,OutputFile),
	assert(autotest_tests(File)),
	error('Wrote to file: '),
	error(OutputFile),
	error_nl,
	fail.
generate_tests_files(AllMPATests) :-
	write_autotest_file,
	write_consult_file.

generate_tests_file(MPATests) :-
	MPATests = [[File,M,P,A],Tests],
	write_term(:- begin_tests(File),[quoted(true)]),
	write('.'), nl, nl,
	member(Test,Tests),
	write_term(Test,[quoted(true)]),
	write('.'), nl, nl,
	fail.

generate_tests_file(MPATests) :-
	MPATests = [[File,M,P,A],Tests],
	write_term(:- end_tests(File),[quoted(true)]),
	write('.'), nl,
	true.
	
wotp(Goal,Atom) :-
	(   with_output_to_predicate([X]>>(assert(saved_output(X))),call(Goal)) -> true ; true),
	saved_output(Atom),
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

goal_expansion(Goal,aop_advice(Goal)) :-
	should_be_wrapped(Goal).

:- consult('test.pl').


%% :- begin_tests(util2).

%% test(my_view_1):-my_view(b).

%% test(tsScry_1) :-
%% 	wot(tsScry(X = 1),A),
