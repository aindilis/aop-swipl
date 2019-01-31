:- use_module(library(predicate_streams)).

run :-
	with_output_to_predicate({}/[X]>>assert(saved_output(X)),
				 (   write("hi there"),nl,writeln("how are you?"))),
	listing(saved_output/1).

:- run.