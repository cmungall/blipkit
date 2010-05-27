:- module(pkb_to_sim,
	  []).

:- use_module(bio(simmatrix_multiset),[]).
:- use_module(bio(pkb_db)).

simmatrix_multiset:feature_attx(O,As) :-
	organism_phenotype(O,P),
	setof(A,
	      Rel^(phenotype_property_value(P,Rel,A),A\='-'),
	      As).




