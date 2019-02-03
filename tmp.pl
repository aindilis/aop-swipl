write_autotestTests(Files) :-
	member(File,Files),
	write('run_tests('),
	write_term(File,[quoted(true)]),
	write(') :: s.'),
	nl,
	fail.
write_autotestTests :-
	true.
