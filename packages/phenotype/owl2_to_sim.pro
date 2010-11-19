:- module(owl2_to_sim,
	  []).

:- use_module(bio(simmatrix_multiset),[]).
:- use_module(bio(tabling)).
:- use_module(bio(index_util)).

:- use_module(library('thea2/owl2_model')).
:- use_module(library('thea2/owl2_graph_reasoner')).

:- [pkb_exclude_from_analysis].


simmatrix_multiset:atomic_subsumed_by(X,Y) :-
        class_ancestor_over(X,Y1,R), % table this
	\+ exclude(Y1),
        tr_rel_cls(R,Y1,Y).

tr_rel_cls([_,_|_],_,_) :-
        !,
        fail.
%tr_rel_cls([all-R],C,allValuesFrom(R,C)) :-
%        !.
tr_rel_cls([all-_],_,_) :-
        !,
        fail.
tr_rel_cls([inst],_,_) :-
        !,
        fail.
tr_rel_cls([irel-_],_,_) :-
        !,
        fail.
tr_rel_cls([some-'http://www.obofoundry.org/ro/ro.owl#has_proper_part'],_,_) :-
        !,
        fail.
% e.g. hippocampus has_part CA2 - CA2 is not a useful ancestor
tr_rel_cls([some-R],C,someValuesFrom(R,C)) :-
        use_expr_for_prop(R),
        !.
tr_rel_cls(_,C,C).

use_expr_for_prop('http://purl.org/obo/owl/OBO_REL#has_part').


% HOOK: simmatrix
%  use the basic tbox reasoner for graph closure
:- multifile simmatrix_multiset:index_hook/1.
simmatrix_multiset:index_hook(reasoner) :-
	materialize_index(owl2_model:equivalent_to(1,1)),
	%table_pred(owl2_tbox_reasoner:subClassOfT/3), % helps?
	%table_pred(owl2_tbox_reasoner:subClassOfT/2).
        true.

