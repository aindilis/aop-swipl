my_view(X) :-
	write(X).

%%---------------

test :-
	my_member(L,[a,b,c,d]),
	my_view(L),
	fail.
test :-
	true.


run :-
	my_view('testing'),
	test.

%%---------------

test2 :-
	my_member(L,[a,b,c,d]),
	my_view(L),
	fail.
test2 :-
	my_member(L,[a,b,c,d]),
	my_view(L),
	fail.
test2 :-
	true.


run2 :-
	my_view('testing'),
	test2.

%%---------------