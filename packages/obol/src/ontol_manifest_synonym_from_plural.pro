:- module(ontol_manifest_synonym_from_plural,
          []).

:- use_module(bio(ontol_db)).

metadata_db:entity_synonym(Class,Stemmed):-
        plsyn(Class,Stemmed).
metadata_db:entity_synonym_scope(Class,Stemmed,exact):-
        plsyn(Class,Stemmed).

plsyn(C,S):-
        class(C,N),
        atom_concat(N,s,S).
