:- module(genome_bridge_from_ncbigene_ontol,
          []).

:- use_module(genome_db).
:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).

genome_db:gene(X):- inst(X).
genome_db:gene_symbol(X,N):- entity_label(X,N).
