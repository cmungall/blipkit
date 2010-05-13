/* -*- Mode: Prolog -*- */

:- module(obolog_writer_prolog_unreified, [
                                  ]).

:- use_module(bio(dbmeta)).
:- use_module(bio(metadata_db)).
:- use_module(bio(obolog_db)).
:- use_module(bio(bioprolog_util)).
:- use_module(bio(sxpr_parser),[sxpr_prolog/2,prolog_sxpr/2]).

io:redirect_stdout(obolog_prolog).

axiomtemplate('=>'(_,_)).
axiomtemplate('<=>'(_,_)).

io:write_all(obolog_prolog,_,_):-
        write_comment('Automatically generated from CL/KIF'),
        %materialize_formulae,
        % XSB Only
        %format(':- auto_table.~n'),
        forall((relation(T),safe_pred(T,TS)),
               format(':- table ~w/2.~n',[TS])),
        forall((relation(T),safe_pred(T,TS)),
               format(':- table ~w/3.~n',[TS])),
        %forall(axiom(is_a(X,Y)),
        %       write_axiom
        forall((type(T),safe_pred(T,TS)),
               format(':- op(300,fx,~w).~n',[TS])),
        %forall((type(T),safe_pred(T,TS)),
        %       format('~w(_):- fail.~n',[TS])),
        forall((type(T),safe_pred(T,TS)),
               format('~w(fake(~w)).~n',[TS,TS])),
        forall((relation(T),safe_pred(T,TS)),
               format(':- op(300,xfy,~w).~n',[TS])),
        %forall((relation(T),safe_pred(T,TS)),
        %       format('~w(_,_):- fail.~n',[TS])),
        %forall((relation(T),safe_pred(T,TS)),
        %       format('~w(_,_,_):- fail.~n',[TS])),
        forall((relation(T),safe_pred(T,TS)),
               format('~w(f1(~w),f2(~w)).~n',[TS,TS,TS])),
        forall((relation(T),safe_pred(T,TS)),
               format('~w(f1(~w),f2(~w),f3(~w)).~n',[TS,TS,TS,TS])),
        % multifile + tabling doesn't work in XSB?
        %forall((type(T),safe_pred(T,TS)),
        %       format(':- multifile(~w/1).~n',[TS])),
        %forall((relation(T),safe_pred(T,TS)),
        %       format(':- multifile(~w/2).~n',[TS])),
        %forall((relation(T),safe_pred(T,TS)),
        %       format(':- multifile(~w/3).~n',[TS])),
        nl,
        forall(axiomtemplate(A),
               forall(A,
                      write_axiom(obolog_prolog,A))),
        forall(writeable_formula(A),
               write_axiom(obolog_prolog,A)),
        forall(axiom_expanded_recursive(A1,A),
               (   write_comment(A1),
                   write_comment(A),
                   write_axiom(obolog_prolog,A))).

/* TODO
prolog_rewrite( (A :- B), ( AX :- BX) ):-
       A=..[P|Args],
       maplist(expand_funcs(Dict),Args,XArgs),
       AX=..[P|XArgs],
       BX=Dict.
*/
writeable_formula(A):-
        formula(A),
        functor(A,F,Arity),
        \+ datapred(obolog_db,F/Arity).

% transitivity over and under is_a
writeable_formula(  ( =>(and(is_a('?x','?z'),Link),Head)) ):-
        (   all_some(R,_)
        ;   all_some_tr(R,_)
        ;   all_some_all_times(R,_)),
        Link =.. [R,'?z','?y'],
        Head =.. [R,'?x','?y'].
writeable_formula(  ( =>(and(is_a('?z','?y'),Link),Head)) ):-
        (   all_some(R,_)
        ;   all_some_tr(R,_)
        ;   all_some_all_times(R,_)),
        Link =.. [R,'?x','?z'],
        Head =.. [R,'?x','?y'].
% TODO: genus differentia
writeable_formula(  ( =>(and([GCond|DConds]),Head)) ):-
	formula(genus(X,G)),
	findall(DCond,
		(   formula(differentium(X,R,Y)),
		    (	all_some(R,RI)
		    ;	all_some_tr(R,RI)
		    ;	all_some_all_times(R,RI)),
		    DCond1 =.. [RI,'?i','?z'], % TODO
		    DCond2 =.. [Y,'?z'],
		    DCond=(DCond1,DCond2)) 
		),
		DConds),
        Head =.. [X,'?i'],
        GCond =.. [G,'?i'].


write_comment(A):-
        format('% ~w~n',[A]).

write_axiom(obolog_prolog,A):-
        %expr(A,Toks,[]),
        debug(obolog_detail,'axiom: ~w',[A]),
        axiom(A,Toks,[]),
        concat_atom(Toks,S),
        write(S),
        write('.'),
        nl.

%fakeinst(X,F):-
%        sformat(F,'fake_~q',[X]).


:- multifile obolog_db:axiom_expandsxpr/2.
:- multifile obolog_db:axiom_expandsxpr/3.

%obolog_db:axiom_expandsxpr(forall(_,_),fail).

%% TODO: this was breaking e.g. transitive(is_a)
obolog_db:axiom_expandsxpr(is_a(A,B),['=>',
                                     [A,'?x'],
                                     [B,'?x']]).
%obolog_db:axiom_expandsxpr(formula(instance_of(I,T)),[T,I]).
obolog_db:axiom_expandsxpr(instance_of(I,T),[T,I]).  % we will have both reified and unreified instantiation
obolog_db:axiom_expandsxpr('=>'(A,B),'=>'(AX,B),axiom_expanded(A,AX)).
obolog_db:axiom_expandsxpr('=>'(A,B),'=>'(A,BX),axiom_expanded(B,BX)).
%obolog_db:axiom_expandsxpr('=>'(A,B),FuncTerm,ExpFuncTerm):-
%        FuncTerm=..[Func|Args],
%        function(FuncTerm),
%        ExpFuncTerm=..[Func|Args].


        
% TODO: determine if this is an axiom or general formula

jop(and).
jop(or).

axiom('=>'(_X,Y)) --> {logical_expr(Y)},!,['% cannot have exprs on LHS: .'].
axiom('=>'(_X,Y)) --> {hilog_expr(Y)},!,['% no HILOG.'].
axiom('=>'(X,Y)) --> {\+logical_expr(Y),\+hilog_expr(Y)},!,expr(Y),[' :- '],expr(X).
axiom('<=>'(X,Y)) --> !,axiom('=>'(X,Y)),['.\n'],axiom('=>'(Y,X)). % TODO - multiple axioms
axiom(X) --> expr(X). % fact

expr('=>'(_,_)) --> !,[fail].
expr('<=>'(_,_)) --> !,[fail].

/*
%expr('<=>'(X,Y)) --> !,['% ('],expr(X),[' <-> '],expr(Y),[')'].
expr('<=>'(X,Y)) --> expr('=>'(X,Y)).
expr('<=>'(X,Y)) --> expr('=>'(Y,X)).

%expr('<=>'(X,Y)) --> {simple(Y)},expr(Y),[' :- '],expr(X).
%expr('<=>'(X,Y)) --> !,{simple(X)},expr(X),[' :- '],expr(Y).
%expr('=>'(X,Y)) --> {logical_expr(Y)},!,expr(Y),[' :- '],expr(X).
expr('=>'(X,Y)) --> {logical_expr(Y)},!,[fail].
expr('=>'(X,Y)) --> {\+logical_expr(Y)},!,expr(Y),[' :- '],expr(X).
*/

%expr(not(X)) --> !,['not('],brac(X),[')'].
expr(not(_X)) --> !,[fail]. % tabling cannot handle negation..
%expr(exists(_X,Y)) --> !,['\+ \+ '],expr(Y).
expr(exists(_X,_Y)) --> !,[fail]. % 
expr(forall(_X,Y)) --> !,expr(Y). % TODO
expr(X) --> {X=..[Op|L],jop(Op)},!,exprj(Op,L).
expr(X) --> {compound(X),X=..[P|L]},!,{safe_pred(P,Psafe)},[Psafe],['('],exprs(L),[')'].
expr(X) --> {atom_variable(X,V)},!,[V].
expr(X) --> {safe_atom(X,A)},!,[A].
exprs([H]) --> {compound(H)},!,['('],expr(H),[')'].
exprs([H]) --> !,expr(H).
exprs([H|T]) --> expr(H),[','],!,exprs(T).
brac(X) --> !,['('],expr(X),[')'].


varx(Q,L) --> {is_list(L)},!,vars(Q,L).
varx(Q,X) --> {X=..L},!,vars(Q,L). % vars get parsed as terms..
vars(_,[]) --> [].
vars(Q,[H|T]) --> [' ',Q,' '],expr(H),vars(Q,T).



exprj(_Op,[H]) --> !,expr(H).
exprj(Op,[H|T]) --> !,expr(H),opx(Op),exprj(Op,T).

opx(and) --> [', '].
opx(or) --> ['; '].

logical_expr(X):- X=..[Op|_],jop(Op).
logical_expr(not(_)).
logical_expr(exists(_,_)).
logical_expr(forall(_,_)).
logical_expr('=>'(_,_)).
logical_expr('<=>'(_,_)).
hilog_expr(X):- X=..[F|_],atom_variable(F,_).

atom_variable(A,V):-
        atom_concat('?',V1,A),
        !,
        sub_atom(V1,0,1,_,Ch),
        (   Ch @>= 'A',
            Ch @=< 'Z'
        ->  V=V1
        ;   upcase_atom(V1,V)).

safe_pred(P,Safe):-
        builtin(P),
        !,
        atom_concat(P,'_',P2),
        safe_atom(P2,Safe).
safe_pred(P,Safe):-
        safe_atom(P,Safe).

safe_atom(A,A):-
        number(A).

safe_atom(A,A):-
        atom_chars(A,[C1|L]),
        C1 @>= 'a',
        C1 @=< 'z',
        forall(member(C,L),
               safe_char(C)),
        \+ builtin(A),
        !.

safe_atom(A,Safe):-
        concat_atom(Toks,'\'',A),
        concat_atom(Toks,'\'\'',A2),
        sformat(Safe,'\'~w\'',[A2]).

builtin(P):- T=..[P,_],predicate_property(T,built_in).
builtin(P):- T=..[P,_,_],predicate_property(T,built_in).


safe_char(C):- is_alpha(C).
safe_char('_').
safe_char(C):- C @>= '0', C @=< '9'.


