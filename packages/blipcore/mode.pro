/* -*- Mode: Prolog -*- */

:- module(mode,
          [(mode)/1,
           op(1150,fx,(mode)),
           op(800,xfx,when)
          ]).

:- op(800,xfx,when).
:- op(1150,fx,mode).
%:- module_transparent (mode)/1,comment/2.

:- discontiguous user:comment/2.

:- multifile
	system:term_expansion/2.
:- dynamic
	system:term_expansion/2.

:- dynamic
        mode_decl/2,
        throw_decl/2.

% rewrite clause if it has mode defined
system:term_expansion((H:-Body),T2):-
        context_module(Mod),
        mode_decl(Mod,H),
        !,
        term_to_impl_term(H,H1),
        C2=(H1:-Body),
        (   throw_decl(Mod,H)
        ->  T2=C2
        ;   functor(H,F,A),
            functor(H2,F,A),
            T2=[(H2:-throw(mode_error(H2))),C2],
            assert(throw_desc(Mod,H))),
        debug(term_expansion,'rewrite ~w => ~w',[(H:-Body),T2]).
system:term_expansion(H,T2):-
        context_module(Mod),
        mode_decl(Mod,H),
        !,
        term_to_impl_term(H,H1),
        (   throw_decl(Mod,H)
        ->  T2=H1
        ;   functor(H,F,A),
            functor(H2,F,A),
            T2=[(H2:-throw(mode_error(H2))),H1],
            assert(throw_desc(Mod,H))),
        debug(term_expansion,'rewrite ~w => ~w',[H,T2]).

system:term_expansion((:- comment(PredDecl,Comments)),
                    [   (:- discontiguous(comment/2)),
                        comment(PredDecl,Comments)]).

%% mode(Mode)
% mode declaration; eg :- mode foo(X,Y) is det when (var(X),nonvar(Y))
system:term_expansion((:-mode(T is M when W)),
                    Clauses):-
        (   mode_checking_on
        ->  context_module(Mod),
            assert(mode_decl(Mod,T)),
            term_to_impl_term(T,T1),
            functor(T,F,A),
            functor(T1,F1,A),
            discontiguous(F/A),
            discontiguous(F1/A),
            make_mode(T,T1,M,W,Clauses)
        ;   Clauses=[]).

% eg :- mode foo(+,-) is det
system:term_expansion((:-mode(T is M)),
                    Clauses):-
        (   mode_checking_on
        ->  context_module(Mod),
            T=..[F|ATypes],           % eg [foo,+,-]
            length(ATypes,Arity),
            functor(T0,F,Arity), % eg foo(X,Y) /vars
            term_to_impl_term(T0,T1), % eg foo_Impl___(X,Y)
            T0=..[F|Args],
            assert(mode_decl(Mod,T0)),
            mode_args_to_when(ATypes,Args,W),
            functor(T1,F1,_),
            discontiguous(F/Arity),
            discontiguous(F1/Arity),
            make_mode(T0,T1,M,W,Clauses)
        ;   Clauses=[]).

mode_args_to_when([],[],true).
mode_args_to_when([M],[A],W):-
        !,
        mode_arg_to_when(M,A,W).
mode_args_to_when([M|ML],[A|AL],WS2):-
        mode_arg_to_when(M,A,W),
        !,
        mode_args_to_when(ML,AL,WS),
        combine_goals(W,WS,WS2).

mode_arg_to_when((+),A,nonvar(A)).
mode_arg_to_when((-),A,var(A)).
mode_arg_to_when((?),_,true).
mode_arg_to_when(M,_,_):- throw(bad_mode(M)).

combine_goals(true,WS,WS):- !.
combine_goals(W,true,W):- !.
combine_goals(W,WS,(W,WS)):- !.


mode_checking_on:-
        debugging(mode),
        !.
mode_checking_on:-
        getenv('PROLOG_MODE_CHECKING',X),
        \+ X='',
        \+ X='0',
        !.

make_mode(T,T1,det,W,(T :- (  W
                           -> !,
                               (T1 *-> true ;
                                   throw(mode_error(det_call_failed(T))))))).
make_mode(T,T1,semidet,W,(T :- (W -> !, T1))).
%make_mode(T,T1,nondet,W,(T :- (W -> !, T1 ; true,!))).
make_mode(T,T1,nondet,W,(T :- (W -> !, T1 ))).
make_mode(T,T1,multi,W,(T :- (W -> !, T1 *-> true ; throw(mode_error(multi_call_failed(T)))))).

term_to_impl_term(T,T1):-
        T=..[F|Args],
        concat_atom([F,'Impl___'],F1),
        T1=..[F1|Args].

%mode(Heads):-
%	throw(error(context_error(nodirective, mode(Heads)), _)).
mode(_).
comment(P,C):-
	throw(error(context_error(nodirective, comment(C,P)), _)).

/** <module> run-time mode checking

  ---+ Synopsis

  ==
  :- use_module(bio(mode)).
  :- debug(mode).

  % usage #1
  :- mode foo(X,Y) is det when nonvar(X).
  foo(X,Y):-
        Y is X+1.
  
  % usage #2
  :- mode foo2(+,?) is det.
  foo2(X,Y):-
        Y is X+1.
  
  ==

  ---+ Description

  mode checking can be turned on by calling debug(mode) - this must be
done BEFORE module compilation (as the mode system works by
compile-time term rewriting). You can also turn on checking by settimg
the env var PROLOG_MODE_CHECKING to a non-empty string

  remember, mode checking will make programs run slower.

  mode errors are only caught at run-time

  ---+ Debugging

  * =|debug(term_expansion)|= shows how terms are rewritten to do dynamic mode checking
  * =|debug(mode)|= dynamic mode checking on

  note that these must be set BEFORE compilation

  ---+ TODO/BUGS

  semidet and nondet are treated equivalently - if a semidet predicate
call succeeds more than once, it will NOT complain
  
**/
