:- module(ontol_manifest_synonym_from_fma,
          []).

:- use_module(bio(ontol_db)).

metadata_db:entity_synonym(Class,Stemmed):-
        plsyn(Class,Stemmed).
metadata_db:entity_synonym_scope(Class,Stemmed,exact):-
        plsyn(Class,Stemmed).

plsyn(C,S):-
        class(C,N),
        downcase_atom(N,Nd),
        concat_atom(Toks,' ',Nd),
        append(L1,[layer,of|T],Toks),
        flatten([T,L1,layer],Toks2),
        concat_atom(Toks2,' ',S).
