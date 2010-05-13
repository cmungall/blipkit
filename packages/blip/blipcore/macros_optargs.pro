/* -*- Mode: Prolog -*- */



:- module(macros_optargs,
          [(optargs)/1,
           op(1150,fx,(optargs))]).

:- op(1150,fx,optargs).
:- module_transparent (optargs)/1.

:- multifile
	system:term_expansion/2.

system:term_expansion((:- optargs(Preds)),
                    Clauses):-
    !,
    preds_to_optargs(Preds,Clauses).

preds_to_optargs((P,Ps),CL):-
    !,
    optargs_clauses(P,CL1),
    preds_to_optargs(Ps,CL2),
    append(CL1,CL2,CL).
preds_to_optargs(P,CL):-
    optargs_clauses(P,CL).

optargs_clauses(F/A,CL):-
    AMax is A-1,
    findall(C,(between(2,AMax,AThis),optargs_clause(F/AThis,A,C)),CL).

optargs_clause(F/A1,A2,C):-
    functor(T1,F,A1),
    T1=..[F|Args1],
    ADelta is A2-A1,
    findall(_,between(1,ADelta,_),ArgsDelta), % [_,_,_,....]
    append(Args1,ArgsDelta,Args2),
    T2=..[F|Args2],
    C= ( T1:- T2).

optargs(Heads):-
	throw(error(context_error(nodirective, optargs(Heads)), _)).
/** <module>
  @author Chris Mungall
  @version  $Revision: 1.1 $
  @date  $Date: 2006/02/10 23:29:39 $
  @license LGPL

  ---+ Name
  ---++ macros_optargs
- macros for predicates with optional number of arguments

  ---+ Synopsis

  ==
  :- use_module(bio(macros_optargs)).
  :- optargs foo/5.  % automatically creates foo/2, foo/3, foo/4

  foo(a,b,c,d,e).

  demo:-
    forall(foo(X,Y),
           format('foo ~w ~w~n',[X,Y])).
  ==

  ---+ Description

**/