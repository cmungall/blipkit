:- use_module(pkb_db).
:- use_module(bio(ontol_db)).

organism_gene_phenotype_quad(O,G,P,E,Q,D,W) :-
	organism_phenotype_quad(O,P1,(E,Q,D,W)),
	(   atom(P1)
	->  P=P1
	;   P='-'),
	organism_variant_gene(O,G).

organism_gene_species_phenotype_quad(O,G,S,P,E,Q,D,W) :-
	organism_gene_phenotype_quad(O,G,P,E,Q,D,W),
	organism_species(O,S).

	
	



