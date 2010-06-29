:- module(ontol_manifest_synonym_from_tick_anatomy,
          []).

:- use_module(bio(ontol_db)).

% e.g. myeloid progenitor <-  myeloid progenitor cell
metadata_db:entity_synonym(Class,Stemmed):-
        adult_syn(Class,Stemmed).
metadata_db:entity_synonym_scope(Class,Stemmed,exact):-
        adult_syn(Class,Stemmed).

adult_syn(Class,Stemmed):-
        class(Class,Name),
        atom_concat('Adult ',Stemmed,Name).
adult_syn(Class,Stemmed):-
        class(Class,Name),
        atom_concat('adult ',Stemmed,Name).
