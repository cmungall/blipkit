/*
    Author:        Ulrich Neumerkel
    E-mail:        ulrich@complang.tuwien.ac.at
    Copyright (C): 2009 Ulrich Neumerkel
*/

:- module(lambda, [
   		   (^)/3, (^)/4, (^)/5, (^)/6, (^)/7, (^)/8, (^)/9,
		   (\)/1, (\)/2, (\)/3, (\)/4, (\)/5, (\)/6, (\)/7,
		   (+\)/2, (+\)/3, (+\)/4, (+\)/5, (+\)/6, (+\)/7,
		   op(201,xfx,+\)]).

/** <module> Lambda expressions

This library provides lambda expressions to simplify higher order
programming based on call/N.

Lambda expressions are represented by ordinary Prolog terms.
There are two kinds of lambda expressions:

    Free+\X1^X2^ ..^XN^Goal

         \X1^X2^ ..^XN^Goal

The second is a shorthand for t+\X1^X2^..^XN^Goal.

Xi are the parameters.

Goal is a goal or continuation. Syntax note: Operators within Goal
require parentheses due to the low precedence of the ^ operator.

Free contains variables that are valid outside the scope of the lambda
expression. They are thus free variables within.

All other variables of Goal are considered local variables. They must
not appear outside the lambda expression. This restriction is
currently not checked. Violations may lead to unexpected bindings.

In the following example the parentheses around X>3 are necessary.

==
?- use_module(library(lambda)).
?- use_module(library(apply)).

?- maplist(\X^(X>3),[4,5,9]).
true.
==

In the following X is a variable that is shared by both instances of
the lambda expression. The second query illustrates the cooperation of
continuations and lambdas. The lambda expression is in this case a
continuation expecting a further argument.

==
?- Xs = [A,B], maplist(X+\Y^dif(X,Y), Xs).
Xs = [A, B],
dif(X, A),
dif(X, B).

?- Xs = [A,B], maplist(X+\dif(X), Xs).
Xs = [A, B],
dif(X, A),
dif(X, B).
==

The following queries are all equivalent. To see this, use
the fact f(x,y).
==
?- call(f,A1,A2).
?- call(\X^f(X),A1,A2).
?- call(\X^Y^f(X,Y), A1,A2).
?- call(\X^(X+\Y^f(X,Y)), A1,A2).
?- call(call(f, A1),A2).
?- call(f(A1),A2).
?- f(A1,A2).
A1 = x,
A2 = y.
==

Further discussions
http://www.complang.tuwien.ac.at/ulrich/Prolog-inedit/ISO-Hiord

@tbd Static expansion similar to apply_macros.
@author Ulrich Neumerkel
*/

:- meta_predicate no_hat_call(0).

:- meta_predicate
	^(?,0,?),
	^(?,1,?,?),
	^(?,2,?,?,?),
	^(?,3,?,?,?,?),
	^(?,4,?,?,?,?,?).

^(V1,Goal,V1) :-
   no_hat_call(Goal).
^(V1,Goal,V1,V2) :-
   call(Goal,V2).
^(V1,Goal,V1,V2,V3) :-
   call(Goal,V2,V3).
^(V1,Goal,V1,V2,V3,V4) :-
   call(Goal,V2,V3,V4).
^(V1,Goal,V1,V2,V3,V4,V5) :-
   call(Goal,V2,V3,V4,V5).
^(V1,Goal,V1,V2,V3,V4,V5,V6) :-
   call(Goal,V2,V3,V4,V5,V6).
^(V1,Goal,V1,V2,V3,V4,V5,V6,V7) :-
   call(Goal,V2,V3,V4,V5,V6,V7).

:- meta_predicate
	\(0),
	\(1,?),
	\(2,?,?),
	\(3,?,?,?),
	\(4,?,?,?,?),
	\(5,?,?,?,?,?),
	\(6,?,?,?,?,?,?).

\(FC) :-
   copy_term_nat(FC,C),no_hat_call(C).
\(FC,V1) :-
   copy_term_nat(FC,C),call(C,V1).
\(FC,V1,V2) :-
   copy_term_nat(FC,C),call(C,V1,V2).
\(FC,V1,V2,V3) :-
   copy_term_nat(FC,C),call(C,V1,V2,V3).
\(FC,V1,V2,V3,V4) :-
   copy_term_nat(FC,C),call(C,V1,V2,V3,V4).
\(FC,V1,V2,V3,V4,V5) :-
   copy_term_nat(FC,C),call(C,V1,V2,V3,V4,V5).
\(FC,V1,V2,V3,V4,V5,V6) :-
   copy_term_nat(FC,C),call(C,V1,V2,V3,V4,V5,V6).

:- meta_predicate
	+\(?,1,?),
	+\(?,2,?,?),
	+\(?,3,?,?,?),
	+\(?,4,?,?,?,?),
	+\(?,5,?,?,?,?,?),
	+\(?,6,?,?,?,?,?,?).

+\(GV,FC) :-
   copy_term_nat(GV+FC,GV+C),no_hat_call(C).
+\(GV,FC,V1) :-
   copy_term_nat(GV+FC,GV+C),call(C,V1).
+\(GV,FC,V1,V2) :-
   copy_term_nat(GV+FC,GV+C),call(C,V1,V2).
+\(GV,FC,V1,V2,V3) :-
   copy_term_nat(GV+FC,GV+C),call(C,V1,V2,V3).
+\(GV,FC,V1,V2,V3,V4) :-
   copy_term_nat(GV+FC,GV+C),call(C,V1,V2,V3,V4).
+\(GV,FC,V1,V2,V3,V4,V5) :-
   copy_term_nat(GV+FC,GV+C),call(C,V1,V2,V3,V4,V5).
+\(GV,FC,V1,V2,V3,V4,V5,V6) :-
   copy_term_nat(GV+FC,GV+C),call(C,V1,V2,V3,V4,V5,V6).


%% no_hat_call(:Goal)
%
% Like call, but issues an error for a goal (^)/2.  Such goals are
% likely the result of an insufficient number of arguments.

no_hat_call(MGoal) :-
   strip_module(MGoal, _, Goal),
   (  nonvar(Goal),
      Goal = (_^_)
   -> throw(error(representation_error(lambda_parameters),_))
   ;  call(MGoal)
   ).
