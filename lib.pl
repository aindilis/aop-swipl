tester(Goal,Result,Output) :-
	with_output_to(atom(Atom),(call(Goal) -> Result = true ; Result = fail)),
	(   Output \== Atom -> (error_nl,error('ERROR: Expected: '),error(Output),error_nl,error('	But got: '),error(Atom),error_nl,fail) ; true).

my_member(_,[]) :-
	fail.
my_member(A,[H|R]) :-
	A = H ;
	my_member(A,R).

errorq(Item) :-
	with_output_to(user_error,write_term(Item,[quoted(true)])).

error(Item,Options) :-
	with_output_to(user_error,write_term(Item,Options)).

error(Item) :-
	with_output_to(user_error,write_term(Item,[])).

error_nl :-
	with_output_to(user_error,nl).
