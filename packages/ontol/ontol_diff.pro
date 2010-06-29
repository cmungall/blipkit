:- module(ontol_diff,
	  [
	   uniq_subclass/3
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
