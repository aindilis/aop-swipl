my_view(X) :-
	error(X).

test :-
	member(L,[a,b,c,d]),
	my_view(L),
	fail.
test.

%% run :-
%% 	findall(_,test,Ls),
%% 	my_view([ls,Ls]).

run :-
	test.