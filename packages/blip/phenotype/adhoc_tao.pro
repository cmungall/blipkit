:- use_module(pkb_db).
:- use_module(bio(index_util)).
:- use_module(bio(tabling)).
:- use_module(bio(ontol_db)).
:- use_module(bio(homol_db)).
:- use_module(bio(homol_bridge_from_ontol)).

indexp :-
	%table_pred(ontol_db:bf_parentRT/2),
	materialize_index(ontol_db:bf_parentRT(1,1)),
	materialize_index(pkb_db:phenotype_property_value(1,0,1)),
	materialize_index(pkb_db:organism_phenotype(1,1)).

mutant_phenotype_homology(O,P,X,Y) :-
	homologous_toS(X,Y),
	entity_xref(X,A), % e.g. zfin
	phenotype_property_value(P,_,A), % consider indexing
	organism_phenotype(O,P).	 % consider indexing

mutant_phenotype_homologyT(O,P,X,Y) :-
	homologous_toS(X,Y),
	entity_xref(X,A), % e.g. zfin
	bf_parentRT(A2,A),
	debug(foo,' ch: ~w',[A-A2]),
	phenotype_property_value(P,_,A2), % consider indexing
	organism_phenotype(O,P).	  % consider indexing
