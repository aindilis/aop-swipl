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

my_member(_,[]) :-
	fail.
my_member(A,[H|R]) :-
	A = H ;
	my_member(A,R).

should_be_wrapped(Predicate,R) :-
	(   Predicate = ',' ->
	    true ; %% maplist([A]>>(aop_advice(A)),R) ; 
	    (
	     not(my_member(Predicate,[consult,module,use_module,set_prolog_flag,member_,debug_print_hook,assertion_failed,pred_option,locate_clauses,pred_option,pred_option,pred_option,pop_compile_operators,push_compile_operators,push_compile_operators,quasi_quotation_syntax,alternate_syntax,quasi_quotation_syntax,xref_open_source,xref_close_source,xref_source_identifier,file_search_path,prolog_file_type,goal_expansion,prolog_predicate_name,prolog_clause_name,prolog_clause_name,with_output_to,open,listing,is,!])),
	     not(my_member(Predicate,[error,error_nl,wot,should_be_wrapped,do_aop_code_before,do_aop_code_after,goal_expansion,aop_advice,write_data_to_file,writeq_data_to_file,generate_tests_for_goal])),
	     not(my_member(Predicate,[nl,write,write_term,write_data_to_file,writeq_data_to_file,string_match_p]))
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
	with_output_to_predicate([X]>>(
				       asserta(saved_output(X))
				      ),
				 (   call(Goal) *-> (Result = true; (do_aop_code_redo(Goal),fail) ) ; Result = fail )),
	findall(X,saved_output(X),Xs),
	retractall(saved_output(_)),
	atomic_list_concat(Xs,'',SavedOutput),
	error([savedOutput,SavedOutput]),error_nl,
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
	retractall(test_data(X,Y,Z)),
	my_mode(Mode).

generate_tests_files(AllMPATests) :-
	member(MPATests,AllMPATests),
	MPATests = [[File,M,P,A],Tests],
	string_match_p(File,'^/var/lib/myfrdcsa/.*\\.pl$'),
	atomic_list_concat([File,'t'],'',OutputFile),
	error_nl,
	wotp(generate_tests_file(MPATests),Atom),
	write_data_to_file(Atom,OutputFile),
	assert(seen(File)),
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

%% term_expansion(Term,aop_advice(Term2)) :-
%% 	Term =.. [P|R],
%% 	Term2 =.. [P|R],
%% 	should_be_wrapped(P,R).

%% goal_expansion(Goal,aop_advice(Goal2)) :-
%% 	Goal =.. [P|R],
%% 	Goal2 =.. [P|R],
%% 	should_be_wrapped(P,R).

goal_expansion(Goal,aop_advice(Goal)) :-
	Goal =.. [P|R],
	(   P = ',' -> (member(R1,R), R1 =.. [P2|R2], should_be_wrapped(P2,R2), Goal2 =.. [P2|R2], goal_expansion(Goal2,aop_advice(Goal2)), error_nl, fail) ; should_be_wrapped(P,R)).

:- consult('test.pl').

:- consult('t/factorial.pl').

do :-
	generate_tests_for_goal(factorial(5,X)),
	generate_tests_for_goal(run2).




%% tester(Goal,Result,Atom) :-
%% 	with_output_to(atom(Atom),(call(Goal) -> Result = true ; Result = fail)).

%% :-begin_tests('/var/lib/myfrdcsa/codebases/minor/aop-swipl/test.pl').

%% test(my_view__1) :-
%% 	tester(my_view(a),true,'a').

%% test(my_view__1):-
%% 	tester(my_view(b),true,'b').

%% :-end_tests('/var/lib/myfrdcsa/codebases/minor/aop-swipl/test.pl').
