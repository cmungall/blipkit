:- use_module(pkb_db).

species_used(S) :- setof(S,O^organism_species(O,S),SL),member(S,SL).

species_pair(S1,S2) :-
	species_used(S1),
	species_used(S2).

organism_best_hit_in_species(A,B,Sp,CI,CU,Sc) :-
	organism(A),
	feature_pair_ci_cu_simj(A,B,CI,CU,Sc),
	A\=B,
	organism_species(B,Sp),
	\+ ((	feature_pair_ci_cu_simj(A,B2,_,_,Sc2),
	     organism_species(B2,Sp),
	     Sc2 > Sc)).

