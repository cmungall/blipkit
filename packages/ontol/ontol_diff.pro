:- module(ontol_diff,
	  [
	   uniq_subclass/3,
           diff_label/5
	   ]).

:- use_module(ontol_db).
:- use_module(bio(dbmeta)).

uniq_subclass(X,Y,S) :-
	G=subclass(X,Y),
	G,
	fact_clausesource(G,S),
	fact_clausesource(class(X),S2),
	S2\=S,
	fact_clausesource(class(Y),S2),
	\+ fact_clausesource(G,S2).

diff_label(X,L1,L2,S1,S2) :-
        entity_label(X,L1),
        fact_clausesource(entity_label(X,L1),S1),
        entity_label(X,L2),
        L1\=L2,
        fact_clausesource(entity_label(X,L2),S2).

        