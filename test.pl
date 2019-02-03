my_view(X) :-
	write(X).

%%---------------

test :-
	member(L,[a,b,c,d]),
	my_view(L),
	fail.
test.


run :-
	write('testing'),
	test.

%%---------------

test2 :-
	member(L,[a,b,c,d]),
	my_view(L),
	fail.
test2 :-
	member(L,[a,b,c,d]),
	my_view(L),
	fail.
test2.


run2 :-
	write('testing'),
	test2.

%%---------------