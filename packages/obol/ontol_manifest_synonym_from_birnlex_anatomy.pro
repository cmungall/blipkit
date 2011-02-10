:- module(ontol_manifest_synonym_from_birnlex_anatomy,
          []).

:- use_module(bio(ontol_db)).

metadata_db:entity_synonym(Class,Stemmed):-
        plsyn(Class,Stemmed).
metadata_db:entity_synonym_scope(Class,Stemmed,exact):-
        plsyn(Class,Stemmed).

plsyn(C,S):-
        class(C,N),
        repl(From,To),
        atom_concat(From,X,N),
        atom_concat(To,X,S).

repl('regional part of','segment of').
repl('lobe parts of','segment of').
repl('predominantly white regional part','white matter layer').
repl('predominantly gray regional part','grey matter layer').
