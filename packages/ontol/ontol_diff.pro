:- module(ontol_diff,
	  [
	   uniq_subclass/4,
           diff_label/5
	   ]).

:- use_module(ontol_db).
:- use_module(bio(dbmeta)).

src_subclass(S,X,Y) :-
        subclass(X,Y),
        fact_clausesource(subclass(X,Y),S).

src_subclassT(S,X,Y) :-
        src_subclass(S,X,Y).
src_subclassT(S,X,Y) :-
        src_subclass(S,X,Z),
        src_subclassT(S,Z,Y).

uniq_subclass(X,Y,Info,S) :-
        src_subclass(S,X,Y),
	fact_clausesource(class(X),S2),
	S2\=S,
	fact_clausesource(class(Y),S2),
	\+ src_subclass(S2,X,Y),
	(   src_subclassT(S2,X,Y)
        ->  sformat(Info,'INDIRECT_in_~w',[S2])
        ;   Info='UNIQUE').


diff_label(X,L1,L2,S1,S2) :-
        entity_label(X,L1),
        fact_clausesource(entity_label(X,L1),S1),
        entity_label(X,L2),
        L1\=L2,
        fact_clausesource(entity_label(X,L2),S2),
        S1 @< S2. % asym



        