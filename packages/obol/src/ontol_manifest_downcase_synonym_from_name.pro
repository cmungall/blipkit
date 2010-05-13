:- module(ontol_manifest_downcase_synonym_from_name,
          []).

:- use_module(bio(metadata_db)).
:- use_module(bio(ontol_db)).

stemmed_synonym(Class,Stemmed):-
        class(Class,N),
        downcase_atom(N,Stemmed).
metadata_db:entity_synonym_scope(E,S,exact):- stemmed_synonym(E,S).
metadata_db:entity_synonym(E,S):- stemmed_synonym(E,S).
