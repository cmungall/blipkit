:- module(pkb_to_sim,
	  []).

:- use_module(library('thea2/owl2_tbox_reasoner')).
:- use_module(library('thea2/owl2_model')).
:- use_module(bio(simmatrix_multiset),[]).
:- use_module(bio(tabling)).
:- use_module(bio(index_util)).
:- use_module(bio(pkb_db)).

simmatrix_multiset:feature_attx(O,As) :-
	organism_phenotype(O,P),
	setof(A,
	      Rel^(phenotype_property_value(P,Rel,A),A\='-'),
	      As).


:- module_transparent simmatrix:generate_term_indexes_hook/1.
:- multifile simmatrix:generate_term_indexes_hook/1.
simmatrix:generate_term_indexes_hook(organism_phenotype) :-
	ensure_loaded(bio(simmatrix)),
	%materialize_index(owl2_model:equivalent_to(1,1)),
	table_pred(owl2_tbox_reasoner:subClassOfT/3), % helps?
	table_pred(owl2_tbox_reasoner:subClassOfT/2),
	simmatrix:generate_term_indexes(O,A,
					(   pkb_db:organism_phenotype(O,P),
					    pkb_db:phenotype_property_value(P,_,A1),
					    owl2_tbox_reasoner:subClassOfRT(A1,A))).

/*
simmatrix:generate_term_indexes_hook(organism_phenotype) :-
	ensure_loaded(bio(ontol_db)),
	ensure_loaded(bio(simmatrix)),
	generate_term_indexes(O,A,
			      (	  pkb_db:organism_phenotype(O,P),
				  pkb_db:phenotype_property_value(P,_,A1),
				  ontol_db:bf_parentRT(A1,A))).
*/
	

