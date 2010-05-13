:- module(genehack,[w/1]).

				%:- use_module(library('thea2/owl2_model'),[labelAnnotation_value/2]).

%:- use_module(bio(metadata_db),[]).
:- use_module(bio(pkb_db),[organism_label/2]).
%:- use_module(bio(genome_db),[]).

w(metadata_db:entity_label(G,G)) :-
	organism_species_gene(_,_,_,G,_).

w(genome_db:gene(G)) :-
	organism_species_gene(_,_,_,G,_).

w(pkb_db:organism_variant_gene(O,G)) :-
	organism_species_gene(_,ON,_,G,_),
	organism_label(O,ON).

