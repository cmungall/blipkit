:- use_module(library('thea2/owl2_graph_reasoner')).
:- use_module(library('thea2/owl2_model')).
:- use_module(bio(simmatrix_multiset),[]).
:- use_module(bio(tabling)).
:- use_module(bio(index_util)).
:- use_module(bio(pkb_db)).

:- [pkb_exclude_from_analysis].

% HOOK: simmatrix
:- module_transparent simmatrix:generate_term_indexes_hook/1.
:- multifile simmatrix:generate_term_indexes_hook/1.
simmatrix:generate_term_indexes_hook(organism_phenotype) :-
	ensure_loaded(bio(simmatrix)),
	materialize_index(owl2_model:equivalent_to(1,1)),
	table_pred(owl2_graph_reasoner:class_ancestor_over/3),
        labelAnnotation_value(PC,'Phenotype'),
        setof(P,subClassOf(P,PC),Ps),
        debug(foo,'DONE getting org-atts (directed)',[]),
	simmatrix:generate_term_indexes(P,A,
					(   member(P,Ps),
                                            owl2_graph_reasoner:class_ancestor_over(P,A,_),
                                            \+ exclude(A))).


					    

