/* -*- Mode: Prolog -*- */

:- module(obolog_writer_prover9, [
                                  ]).

:- use_module(bio(dbmeta)).
:- use_module(bio(metadata_db)).
:- use_module(bio(obolog_db)).
:- use_module(bio(bioprolog_util)).
:- use_module(bio(sxpr_parser),[sxpr_prolog/2,prolog_sxpr/2]).

io:redirect_stdout(prover9).
io:redirect_stdout(prover9_minimal).
io:redirect_stdout(prover9(_)).

axiomtemplate('=>'(_,_)).
axiomtemplate('<=>'(_,_)).

io:write_all(prover9,_,_):-
        write_comment('Automatically generated from CL/KIF'),
        %forall(axiomtemplate(A),
        %       forall(A,
        %              write_axiom(prover9,A))),
	writeln('formulas(sos).'),
        forall(writeable_formula(A),
               write_axiom(prover9,A)),
        forall(axiom_expanded(A1,A),
               (   debug(obolog,'Expanded ~w -> ~w',[A1,A]),
                   write_comment(A1),
                   write_axiom(prover9,A))),
	writeln('end_of_list.').

io:write_all(prover9_minimal,_,_):-
	writeln('formulas(sos).'),
        forall(axiom_expanded(A1,A),
               (   debug(obolog,'Expanded ~w -> ~w',[A1,A]),
                   write_comment(A1),
                   write_axiom(prover9,A))),
        forall(ontol_db:logicalformula(_,F,'Prover9'),
               format('~w.~n',[F])),
	writeln('end_of_list.').


io:write_all(prover9(Theorem),_,_):-
        write_comment('Automatically generated from CL/KIF'),
        %forall(axiomtemplate(A),
        %       forall(A,
        %              write_axiom(prover9,A))),
	writeln('formulas(sos).'),
        forall((writeable_formula(A),\+theorem(A,_)),
               write_axiom(prover9,A)),
        forall(axiom_expanded(A1,A),
               (   write_comment(A1),
                   write_axiom(prover9,A))),
	writeln('end_of_list.'),
	writeln('formulas(goals).'),
        forall((writeable_formula(A),theorem(A,Theorem)),
               write_axiom(prover9,A)),
	writeln('end_of_list.').

theorem(A,Theorem):-
	formula_comment(A,C),
	atom_concat('Theorem',X,C),
	concat_atom([_,Theorem|_],' ',X).



writeable_formula(A):-
        formula(A),
        functor(A,F,Arity),
        \+ datapred(obolog_db,F/Arity).

write_comment(A):-
        format('% ~w~n',[A]).

write_axiom(prover9,A):-
        expr(A,Toks,[]),
        concat_atom(Toks,S),
        write(S),
        write('.'),
        nl,
        nl.


jop(and).
jop(or).

expr('<=>'(X,Y)) --> !,['('],expr(X),[' <-> '],expr(Y),[')'].
expr('=>'(X,Y)) --> !,['('],expr(X),[' -> '],expr(Y),[')'].
expr('='(X,Y)) --> !,['('],expr(X),[' = '],expr(Y),[')'].
expr(not(X)) --> !,['-'],brac(X).
expr(exists(X,Y)) --> !,varx(exists,X),[' '],brac(Y).
expr(forall(X,Y)) --> !,varx(all,X),[' '],brac(Y).
expr(X) --> {X=..[Op|L],jop(Op)},!,exprj(Op,L).
expr(X) --> {compound(X),X=..[P|L]},!,predsym(P,L),['('],exprs(L),[')'].
expr(X) --> {atom_variable(X,V)},!,[V].
expr(X) --> {safe_atom(X,A)},!,[A].
exprs([H]) --> !,expr(H).
exprs([H|T]) --> expr(H),[','],!,exprs(T).
brac(X) --> !,['('],expr(X),[')'].


varx(Q,L) --> {is_list(L)},!,vars(Q,L).
varx(Q,X) --> {X=..L},!,vars(Q,L). % vars get parsed as terms..
vars(_,[]) --> [].
vars(Q,[H|T]) --> [' ',Q,' '],expr(H),vars(Q,T).

% always use variable arity
predsym(P,L) --> {length(L,N)},!,[P],[N].
%predsym(P,L) --> {variable_arity(P),length(L,N)},!,[P],[N].
%predsym(P,_) --> [P].


exprj(_Op,[H]) --> !,expr(H).
exprj(Op,[H|T]) --> !,expr(H),opx(Op),exprj(Op,T).

opx(and) --> [' & '].
opx(or) --> [' | '].

atom_variable(A,V):-
        atom_concat('?',V1,A),
        !,
        sub_atom(V1,0,1,_,Ch),
        (   Ch @>= 'a',
            Ch @=< 'z'
        ->  V=V1
        ;   atom_concat('u',V1,V)).

safe_atom(A,A):-
        atom_chars(A,[C1|L]),
        C1 @>= 'A',
        C1 @=< 'Z',
        forall(member(C,L),
               safe_char(C)),
        !.

safe_atom(A,Safe):-
        concat_atom(Toks,'"',A),
        concat_atom(Toks,'\\"',A2),
        sformat(Safe,'"~w"',[A2]).
        
safe_char(C):- is_alpha(C).
safe_char('_').
safe_char(C):- C @>= '0', C @=< '9'.


