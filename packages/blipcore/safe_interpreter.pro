:- module(safe_interpreter,
          [
           safe_call/1,
           safe/1
           ]).

:- use_module(dbmeta,[datapred/2,is_pure_pred/1]).

safe_call(Goal):-
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
safe1((A -> B ; C)) :-
        safe(A),
        safe(B),
        safe(C).
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
        

safe_predicate(true/0).
safe_predicate(fail/0).
safe_predicate(member/2).
safe_predicate(=/2).
safe_predicate(concat_atom/2).
safe_predicate(atom_concat/3).
safe_predicate(P) :- datapred(_,P).
safe_predicate(P/2) :- comparison(P).
safe_predicate(P) :- is_pure_pred(P). % hook

comparison(<).
comparison(=<).
comparison(=).
comparison(>).
comparison(>=).

comparison(@<).
comparison(@=<).
comparison(@=).
comparison(@>).
comparison(@>=).

%% metapred(?Pred,?Args,?UnsafeArgs)
% UnsafeArgs must be ground and safe
metapred(forall,[X,Y],[X,Y]).
metapred(setof,[_,G,_],[G]).
metapred(bagof,[_,G,_],[G]).
metapred(findall,[_,G,_],[G]).
metapred(solutions,[_,G,_],[G]).
metapred(aggregate,[_,_,G,_],[G]).

%:- use_module(library(plunit)).




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
