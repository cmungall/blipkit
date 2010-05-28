:- module(owl2_to_sim,
	  []).

:- use_module(bio(simmatrix_multiset),[]).
:- use_module(bio(tabling)).
:- use_module(bio(index_util)).

:- use_module(library('thea2/owl2_tbox_reasoner')).

simmatrix_multiset:subsumed_by(X,Y) :-
	%entailed(subClassOfReflexive(X,Y)),
	subClassOfRT(X,Y),
	\+ exclude(Y).

exclude(Class) :-
	atom(Class),
	sub_atom(Class,0,_,_,'http://www.ifomis.org').

:- multifile simmatrix_multiset:index_hook/1.
simmatrix_multiset:index_hook(reasoner) :-
	%table_pred(owl2_basic_reasoner:entailed/1).
	materialize_index(owl2_model:equivalent_to(1,1)),
	table_pred(owl2_tbox_reasoner:subClassOfT/3), % helps?
	table_pred(owl2_tbox_reasoner:subClassOfT/2).


