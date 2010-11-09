:- module(pkb_to_sim,
	  []).

:- use_module(library('thea2/owl2_graph_reasoner')).
:- use_module(library('thea2/owl2_model')).
:- use_module(bio(simmatrix_multiset),[]).
:- use_module(bio(tabling)).
:- use_module(bio(index_util)).
:- use_module(bio(pkb_db)).

% multisets - each attribute set is an ordered collection of property values; e.g. [Q,E]
simmatrix_multiset:feature_attx(O,As) :-
	organism_phenotype(O,P),
	setof(A,
	      Rel^(phenotype_property_value(P,Rel,A),A\='-'),
	      As).


% HOOK: simmatrix
%  lump all phenotype elements for an organism together

:- module_transparent simmatrix:generate_term_indexes_hook/1.
:- multifile simmatrix:generate_term_indexes_hook/1.
simmatrix:generate_term_indexes_hook(organism_phenotype) :-
	ensure_loaded(bio(simmatrix)),
	materialize_index(owl2_model:equivalent_to(1,1)),
	table_pred(owl2_graph_reasoner:class_ancestor_over/3),
        debug(foo,'getting org-atts (directed)',[]),
        setof(O-P,pkb_to_sim:organism_phenoprop(O,P),OPs),
        debug(foo,'DONE getting org-atts (directed)',[]),
	simmatrix:generate_term_indexes(O,A,
					(   member(O-A1,OPs),
                                            owl2_graph_reasoner:class_ancestor_over(A1,A,_))).

organism_phenoprop(O,A) :-
        pkb_db:organism_phenotype(O,P),
        pkb_db:phenotype_property_value(P,_,A).

					    

