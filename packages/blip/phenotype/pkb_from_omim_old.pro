:- module(pkb_from_omim,[]).

:- use_module(pkb_db).
:- use_module(phenotype_db).
:- use_module(pkb_from_obo).
:- use_module(bio(tabling)).
:- use_module(bio(metadata_db)).
:- use_module(bio(ontol_db)).
:- use_module(bio(seqfeature_db)).

:- multifile metadata_db:entity_label/2.

% hack: omim records stored as seqfeatures..
pkb_db:organism(Org) :- feature(Org).
pkb_db:organism_species(Org,'NCBITaxon:9696') :- pkb_db:organism(Org).

pkb_db:organism_type(Org,Type) :- organism_species(Org,Type). % TODO - transgene construct etc

pkb_db:organism_role(Org,patient) :- organism(Org). % TODO
