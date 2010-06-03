:- module(pkb_from_omim,[]).

:- use_module(pkb_db).
:- use_module(phenotype_db).
%:- use_module(pkb_from_obo).
:- use_module(bio(metadata_db)).
:- use_module(bio(genome_db)).
:- use_module(bio(ontol_db)).

:- multifile metadata_db:entity_label/2.

metadata_db:entity_label('NCBITaxon:9606^bearer_of(DOID:4)','human with disease').

pkb_db:species_label('NCBITaxon:9606',human).
pkb_db:species(S) :- species_label(S,_).

pkb_db:organism(Org) :- organism_variant_gene(Org,_).

% requires omim2gene
pkb_db:organism_variant_gene(O,G) :-
	organism_disease(O,D),
	disorder2gene(D,G).

/*
pkb_db:organism_variant_gene(O,G) :-
	organism_disease(O,D),
	restriction(D,associated_with,X),
	entity_xref(G,X).
*/

pkb_db:disease_label(D,L) :-
	disease(D),
	entity_label(D,L).

% we can't get this directly from the xml as it is the full omim name
% TEMPORARY
pkb_db:organism_label(O,N) :-
	organism_disease(O,D),
	entity_label(D,N1),
	atom_concat('HUMAN WITH ',N1,N).


% TODO - DRY
pkb_db:disease_gene_variant(O,G,any) :-
	restriction(O,associated_with,OG),
	entity_xref(G,OG).

% uses omim.obo
% 
