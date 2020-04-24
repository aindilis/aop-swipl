# introduction
automatic test generator utilizing swipl goal expansion to wrap predicates (hence the AOP "advise" terminology) in memoizing wrappers.
the memoized results can be saved and used as tests.

# files
#### alt*.pl, alt*.plt
different stages of and alternative implemetations. 

#### alt20.pl
this is the one you should be looking at.

#### data

#### run.sn







# example session
```
andrewdo@ai:/var/lib/myfrdcsa/codebases/minor/automated-legacy-testing/dmiles-aop$ swipl
Welcome to SWI-Prolog (threaded, 64 bits, version 7.7.25)
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software.
Please run ?- license. for legal details.

For online help and background, visit http://www.swi-prolog.org
For built-in help, use ?- help(Topic). or ?- apropos(Word).

?- consult('test1.pl').
true.

?- trace.
true.

[trace]  ?- run.
   Call: (8) run ? 
   Call: (9) test ? 
   Call: (10) lists:member(_3046, [a, b, c, d]) ? 
   Exit: (10) lists:member(a, [a, b, c, d]) ? 
   Call: (10) my_view(a) ? 
   Call: (11) error(a) ? 
^  Call: (12) with_output_to(user_error, write_term(a, [quoted(true)])) ? 
^  Call: (14) write_term(a, [quoted(true)]) ? 
a
^  Exit: (14) write_term(a, [quoted(true)]) ? 
^  Exit: (12) with_output_to(user_error, write_term(a, [quoted(true)])) ? 
   Exit: (11) error(a) ? 
   Exit: (10) my_view(a) ? 
   Call: (10) fail ? 
   Fail: (10) fail ? 
   Redo: (10) lists:member(_3046, [a, b, c, d]) ? 
   Exit: (10) lists:member(b, [a, b, c, d]) ? 
   Call: (10) my_view(b) ? 
   Call: (11) error(b) ? 
^  Call: (12) with_output_to(user_error, write_term(b, [quoted(true)])) ? 
^  Call: (14) write_term(b, [quoted(true)]) ? 
b
^  Exit: (14) write_term(b, [quoted(true)]) ? 
^  Exit: (12) with_output_to(user_error, write_term(b, [quoted(true)])) ? 
   Exit: (11) error(b) ? 
   Exit: (10) my_view(b) ? 
   Call: (10) fail ? 
   Fail: (10) fail ? 
   Redo: (10) lists:member(_3046, [a, b, c, d]) ? 
   Exit: (10) lists:member(c, [a, b, c, d]) ? 
   Call: (10) my_view(c) ? 
   Call: (11) error(c) ? 
^  Call: (12) with_output_to(user_error, write_term(c, [quoted(true)])) ? 
^  Call: (14) write_term(c, [quoted(true)]) ? 
c
^  Exit: (14) write_term(c, [quoted(true)]) ? 
^  Exit: (12) with_output_to(user_error, write_term(c, [quoted(true)])) ? 
   Exit: (11) error(c) ? 
   Exit: (10) my_view(c) ? 
   Call: (10) fail ? 
   Fail: (10) fail ? 
   Redo: (10) lists:member(_3046, [a, b, c, d]) ? 
   Exit: (10) lists:member(d, [a, b, c, d]) ? 
   Call: (10) my_view(d) ? 
   Call: (11) error(d) ? 
^  Call: (12) with_output_to(user_error, write_term(d, [quoted(true)])) ? 
^  Call: (14) write_term(d, [quoted(true)]) ? 
d
^  Exit: (14) write_term(d, [quoted(true)]) ? 
^  Exit: (12) with_output_to(user_error, write_term(d, [quoted(true)])) ? 
   Exit: (11) error(d) ? 
   Exit: (10) my_view(d) ? 
   Call: (10) fail ? 
   Fail: (10) fail ? 
   Redo: (9) test ? 
   Exit: (9) test ? 
   Exit: (8) run ? 
true.









andrewdo@ai:/var/lib/myfrdcsa/codebases/minor/automated-legacy-testing/dmiles-aop$ ./run.sh 
Warning: /var/lib/myfrdcsa/codebases/minor/automated-legacy-testing/dmiles-aop/alt5.pl:37:
	Singleton variables: [Output1]
'   Call: (8) consult(\'test.pl\') ?'
[a,member(_6248,[a,b,c,d])]'   Call: (8) member(_6248,[a,b,c,d]) ?'
'   Exit: (8) member(a,[a,b,c,d]) ?'
[a,(my_view(a),fail)]'   Call: (8) my_view(a),fail ?'
a'   Fail: (8) my_view(a),fail ?'
[a,my_view(_6940)]'   Call: (8) my_view(_6940) ?'
_6940'   Exit: (8) my_view(_6940) ?'
[a,fail]'   Call: (8) fail ?'
'   Fail: (8) fail ?'
'   Exit: (8) consult(\'test.pl\') ?'
Welcome to SWI-Prolog (threaded, 64 bits, version 7.7.25)
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software.
Please run ?- license. for legal details.

For online help and background, visit http://www.swi-prolog.org
For built-in help, use ?- help(Topic). or ?- apropos(Word).

?- run.
'   Call: (8) run ?'
'   Call: (8) test ?'
'   Call: (8) member(_2208,[a,b,c,d]) ?'
'   Exit: (8) member(a,[a,b,c,d]) ?'
'   Call: (8) my_view(a) ?'
a'   Exit: (8) my_view(a) ?'
'   Call: (8) fail ?'
'   Fail: (8) fail ?'
'   Exit: (8) test ?'
'   Exit: (8) run ?'
true.

?- 
```
