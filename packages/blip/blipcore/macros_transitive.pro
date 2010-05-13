/* -*- Mode: Prolog -*- */



:- module(macros_transitive,
          [(transitive)/1,
           op(1150,fx,(transitive))]).

:- op(1150,fx,transitive).
:- module_transparent (transitive)/1.

:- multifile
	system:term_expansion/2.

system:term_expansion((:- transitive(Preds)),
               Clauses):-
        !,
        preds_to_transitive(Preds,Clauses).

preds_to_transitive((P,Ps),CL):-
        !,
        transitive_clauses(P,CL1),
        preds_to_transitive(Ps,CL2),
        append(CL1,CL2,CL).
preds_to_transitive(P,CL):-
        transitive_clauses(P,CL).

transitive_clauses(P,CL):-
        findall(C,transitive_clause(P,C),CL).

transitive_clause(P/A,C):-
        functor(T,P,A),
        transitive_clause1(T,C).

% transitive, irreflexive
% X R+ Y:- X R Y
transitive_clause1(T,(THead:- T)):-
        T=..[P|Args],
        concat_atom([P,'T'],P2),
        THead=..[P2|Args].
% X R+ Y:- X R Z and Z R+ Y
transitive_clause1(T,(THead:- TBase,TR)):-
        T=..[P,X,Y|Args],
        concat_atom([P,'T'],P2),
        THead=..[P2,X,Y|Args],
        TBase=..[P,X,Z|Args],
        TR=..[P2,Z,Y|Args].

% transitive reflexive
% X R* X
transitive_clause1(T,(TClause)):-
        T=..[P,X,X|Args],
        concat_atom([P,'RT'],PRT),
        TClause=..[PRT,X,X|Args].
% X R* Y:- X R+ Y
transitive_clause1(T,(THead:- TGoal)):-
        T=..[P|Args],
        concat_atom([P,'RT'],PRT),
        concat_atom([P,'T'],PT),
        THead=..[PRT|Args],
        TGoal=..[PT|Args].

transitive(Heads):-
	throw(error(context_error(nodirective, transitive(Heads)), _)).
/** <module> macros for transitive predicates

  ---+ Synopsis

  ==
  :- use_module(bio(macros_transitive)).
  :- transitive isa/2.  % automatically creates isaT/2, isaRT/2
  
  isa(a,btcon).
  isa(b,c).
  isa(c,d).
  isa(c,e).

  demo:-
    forall(isaT(X,Y),
           format('~w isa+ ~w~n',[X,Y])).
  ==

  ---+ Description

  Generates two rules:

  * <pred>T/2 - transitive closure of T
  * <pred>RT/2 - reflexive transitive closure of T
  
  ---+ TODO

  Do we need this module? It saves a minimal amount of code at the risk of increased abstraction, less visibility to pldoc, ...

**/
