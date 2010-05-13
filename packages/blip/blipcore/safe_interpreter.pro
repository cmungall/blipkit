:- module(safe_interpreter,
          [
           safe_interpret/1,
           safe/1
           ]).

:- use_module(dbmeta,[datapred/2,is_pure_pred/1]).

% misnomer as it doesn't actually interpret..
safe_interpret(Goal):-
        safe(Goal),
        Goal.

safe(X) :- safe1(X),!.

safe1(X) :- var(X),!,fail.

safe1((A,B)) :-
        safe(A),
        safe(B).
safe1((A;B)) :-
        safe(A),
        safe(B).
safe1((A -> B)) :-
        safe(A),
        safe(B).
safe1(\+A) :-
        safe(A).
safe1(_ is _).
safe1(Goal) :-
        Goal=..[Pred|Args],
        length(Args,Arity),
        safe_predicate(Pred/Arity).
safe1(Goal) :-                  % forall etc safe only if goal args safe
        Goal=..[Pred|Args],
        metapred(Pred,Args,SubGoals),
        forall(member(SubGoal,SubGoals),safe(SubGoal)).
        

safe_predicate(P) :- datapred(_,P).
safe_predicate(member/2).
safe_predicate(P/2) :- comparison(P).
safe_predicate(P) :- is_pure_pred(P).

comparison(<).
comparison(=<).
comparison(=).
comparison(>).
comparison(>=).

%% metapred(?Pred,?Args,?UnsafeArgs)
% UnsafeArgs must be ground and safe
metapred(forall,[X,Y],[X,Y]).
metapred(setof,[_,G,_],[G]).
metapred(bagof,[_,G,_],[G]).
metapred(findall,[_,G,_],[G]).
metapred(solutions,[_,G,_],[G]).

%:- use_module(library(plunit)).

:- begin_tests(safe_interpreter,[]).

test(safe) :- safe(member(_,[1,2,3])).
test(safe) :- safe((X=1,X<2)).
test(safe) :- safe(forall(member(X,[1,2,3]),X<4)).
test(unsafe,[fail]) :- safe(writeln(1)).
test(unsafe,[fail]) :- safe(forall(_G,true)).
test(unsafe,[fail]) :- safe(forall(true,_G)).


test(i, all(X == [1,2,3])) :-
        safe_interpret(member(X,[1,2,3])).
test(ni, [fail]) :-
        safe_interpret(forall(member(X,[1,2,3]),
                              format('You should not see this! ~w',[X]))).

test(model) :- safe(subclass(a,b)).
test(model) :- safe(subclassT(a,b)).
        

:- end_tests(safe_interpreter).



/** <module> 

  ---+ Synopsis

==
:- use_module(bio(safe_interpreter)).

% 
demo:-
  nl.
  

==

---+ Details

IMPORTANT: safety could be compromised if the underlying codebase is compromised.
 E.g. nothing to stop 
 

---+ Additional Information

This module is part of blip. For more details, see http://www.blipkit.org

@author  Chris Mungall
@version $Revision$
@see     README
@license License


*/
