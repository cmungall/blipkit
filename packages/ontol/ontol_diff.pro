:- module(ontol_diff,
	  [
           optimize_diff/0,
           src_subclass/3,
           src_subclassT/3,
	   uniq_subclass/4,
           uniq_subclass_with_defs/6,
           diff_label/5,
           diff_def/5
	   ]).

:- use_module(ontol_db).
:- use_module(bio(index_util)).
:- use_module(bio(tabling)).
:- use_module(bio(dbmeta)).

optimize_diff :-
        table_pred(src_subclassT/3),
        materialize_index(uniq_subclass_r(1,0,1)).

src_subclass(S,X,Y) :-
        subclass(X,Y),
        fact_clausesource(subclass(X,Y),S).

src_subclassT(S,X,Y) :-
        src_subclass(S,X,Y).
src_subclassT(S,X,Y) :-
        src_subclass(S,X,Z),
        src_subclassT(S,Z,Y).

% direct unless inferred db is used
uniq_subclass(X,Y,'UNIQUE',S) :-
        src_subclass(S,X,Y),
	fact_clausesource(class(X),S2),
	S2\=S,
	fact_clausesource(class(Y),S2),
	\+ src_subclass(S2,X,Y).


/*
uniq_subclass(X,Y,Info,S) :-
        src_subclass(S,X,Y),
	fact_clausesource(class(X),S2),
	S2\=S,
	fact_clausesource(class(Y),S2),
	\+ src_subclass(S2,X,Y),
        src_subclassT(S2,X,Y),
        sformat(Info,'INDIRECT_in_~w',[S2]).

  uniq_subclass(X,Y,'UNIQUE',S) :-
        uniq_subclass_r(X,Y,S),
        \+ ((uniq_subclass_r(X,Z,S),
             uniq_subclass_r(Z,Y,S))).

% redundant
uniq_subclass_r(X,Y,S) :-
	fact_clausesource(class(X),S),
	fact_clausesource(class(X),S2),
        S\=S2,
                writeln(S-X),

        src_subclassT(S,X,Y),
	fact_clausesource(class(Y),S2),
	\+ src_subclassT(S2,X,Y).

  */

uniq_subclass_with_defs(X,Y,Info,S,DX,DY) :-
        uniq_subclass(X,Y,Info,S),
        (   def(X,DX),
            fact_clausesource(def(X,DX),S)
        ->  true
        ;   DX='n/a'),
        (   def(Y,DY),
            fact_clausesource(def(Y,DY),S)
        ->  true
        ;   DY='n/a').


diff_label(X,L1,L2,S1,S2) :-
        entity_label(X,L1),
        fact_clausesource(entity_label(X,L1),S1),
        entity_label(X,L2),
        L1\=L2,
        fact_clausesource(entity_label(X,L2),S2),
        S1 @< S2. % asym

diff_def(X,L1,L2,S1,S2) :-
        def(X,L1),
        fact_clausesource(def(X,L1),S1),
        def(X,L2),
        L1\=L2,
        fact_clausesource(def(X,L2),S2),
        S1 @< S2. % asym



        