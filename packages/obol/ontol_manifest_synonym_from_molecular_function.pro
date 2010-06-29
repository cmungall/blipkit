:- module(ontol_manifest_synonym_from_molecular_function,
          []).

:- use_module(bio(ontol_db)).

metadata_db:entity_synonym(Class,Stemmed):-
        syn1(Class,Stemmed).
metadata_db:entity_synonym_scope(Class,Stemmed,exact):-
        syn1(Class,Stemmed).

syn1(Class,Stemmed):-
        class(Class,Name),
        atom_concat(Stemmed,' activity',Name).
