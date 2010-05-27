:- module(owl2_to_sim,
	  []).

:- use_module(bio(simmatrix_multiset),[]).
:- use_module(bio(tabling)).

:- use_module(library('thea2/owl2_basic_reasoner')).
:- use_module(library('thea2/owl2_model')).

simmatrix_multiset:subsumed_by(X,Y) :-
	entailed(subClassOfReflexive(X,Y)),
	\+ exclude(Y).

exclude(Class) :-
	atom(Class),
	sub_atom(Class,0,_,_,'http://www.ifomis.org').

:- multifile simmatrix_multiset:index_hook/1.
simmatrix_multiset:index_hook(reasoner) :-
	table_pred(owl2_basic_reasoner:entailed/1).

