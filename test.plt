:-begin_tests('/var/lib/myfrdcsa/codebases/minor/aop-swipl/test.pl').

%% user:my_view/1

test(my_view__1):-tester(my_view(a),true,a).

test(my_view__1):-tester(my_view(b),true,b).

test(my_view__1):-tester(my_view(c),true,c).

test(my_view__1):-tester(my_view(d),true,d).

test(my_view__1):-tester(my_view(testing),true,testing).

%% user:test2/0

test(test2__0):-tester(test2,true,'').

:-end_tests('/var/lib/myfrdcsa/codebases/minor/aop-swipl/test.pl').
