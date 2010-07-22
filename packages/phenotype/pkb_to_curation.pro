
:- module(pkb_to_curation,
	  []).

:- use_module(pkb_db).
:- use_module(bio(metadata_db)).
:- use_module(bio(ontol_db)).
:- use_module(bio(curation_db)).
:- use_module(bio(seqfeature_db)).

metadata_db:entity_label(E,L) :- organism_label(E,L).

seqfeature_db:feature_organism(G,T) :-
        organism_variant_gene(O,G),
        organism_species(O,T).

curation_db:curation_statement(a(G,P),G,has_phenotype,P) :-
        organism_phenotype(O,P),
        atom(P),
        organism_variant_gene(O,G).
