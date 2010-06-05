:- module(phenosim,
	  []
	 ).

:- use_module(bio(simmatrix)).


orgpair_metric_value(F1,F2,S,V) :- fail.

orgpair_simj(F1,F2,SimJ) :-
	feature_pair_simj(F1,F2,SimJ).


% here we lump together any class used to describe the phenotype of an organism
setup(simple) :-
	generate_term_indexes(F,A,(organism_phenotype(F,P),
				   phenotype_property_value(P,_,A),
				   A\='-')).


setup(simple(Ont)) :-
	generate_term_indexes(F,A,(organism_phenotype(F,P),
				   phenotype_property_value(P,_,A),
				   id_idspace(A,Ont))).

setup(precomposed) :-
	generate_term_indexes(F,A,(organism_phenotype(F,A),class(A))).


	 