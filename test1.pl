error(Item) :-
	with_output_to(user_error,write_term(Item,[quoted(true)])).

:- consult('test.pl').
