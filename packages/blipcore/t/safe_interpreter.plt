/* -*- Mode: Prolog -*- */

:- use_module(../safe_interpreter).

:- begin_tests(safe_interpreter,[]).

test(safe) :- safe(member(_,[1,2,3])).
test(safe) :- safe((X=1,X<2)).
test(safe) :- safe(forall(member(X,[1,2,3]),X<4)).
test(unsafe,[fail]) :- safe(writeln(1)).
test(unsafe,[fail]) :- safe(forall(_G,true)).
test(unsafe,[fail]) :- safe(forall(true,_G)).


test(i, all(X == [1,2,3])) :-
        safe_call(member(X,[1,2,3])).
test(ni, [fail]) :-
        safe_call(forall(member(X,[1,2,3]),
                         format('You should not see this! ~w',[X]))).

test(model) :- safe(subclass(a,b)).
test(model) :- safe(subclassT(a,b)).
        

:- end_tests(safe_interpreter).
