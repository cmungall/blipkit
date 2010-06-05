:- module(ontol_bridge_to_sim,
	  []).

:- use_module(ontol_db,[]).
:- use_module(bio(simmatrix_multiset),[]).

simmatrix_multiset:subsumed_by(A,B) :- ontol_db:subclassRT(A,B).

