/* -*- Mode: Prolog -*- */

:- module(seqfeature_bridge_from_ontol,
          [
          ]).

:- use_module(bio(seqfeature_db),[]).
:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).

seqfeature_db:feature_type(F,T) :-
        inst_of(F,TI),
        entity_label(TI,T).

seqfeature_db:feature(F) :-
        seqfeature_db:feature_type(F,_).

seqfeature_db:feature_organism(F,O) :-
        inst_sv(F,encoded_by,O).



