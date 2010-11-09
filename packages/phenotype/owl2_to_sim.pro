:- module(owl2_to_sim,
	  []).

:- use_module(bio(simmatrix_multiset),[]).
:- use_module(bio(tabling)).
:- use_module(bio(index_util)).

:- use_module(library('thea2/owl2_model')).
:- use_module(library('thea2/owl2_graph_reasoner')).

:- [pkb_exclude_from_analysis].


simmatrix_multiset:atomic_subsumed_by(X,Y) :-
        class_ancestor_over(X,Y1,_),
	\+ exclude(Y1),
        Y=Y1.                   % hack - table class_ancestor_over/3 for all Y



% HOOK: simmatrix
%  use the basic tbox reasoner for graph closure
:- multifile simmatrix_multiset:index_hook/1.
simmatrix_multiset:index_hook(reasoner) :-
	materialize_index(owl2_model:equivalent_to(1,1)),
	%table_pred(owl2_tbox_reasoner:subClassOfT/3), % helps?
	%table_pred(owl2_tbox_reasoner:subClassOfT/2).
        true.

