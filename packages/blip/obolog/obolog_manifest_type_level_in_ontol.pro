/* -*- Mode: Prolog -*- */

:- module(obolog_manifest_type_level_in_ontol,[
                  ]).

:- use_module(bio(obolog_db)).
:- use_module(bio(ontol_db),[]).
:- use_module(bio(metadata_db),[]).

% metadata
metadata_db:entity_label(X,L):- label(X,L).

% relations
ontol_db:property(X):- relation(X).

ontol_db:is_transitive(X):- obolog_db:transitive(X).
ontol_db:is_reflexive(X):- reflexive(X).
ontol_db:is_symmetric(X):- symmetric(X).
ontol_db:is_anti_symmetric(X):- anti_symmetric(X).

ontol_db:inverse_of(X,Y):- inverse_of(X,Y).

ontol_db:subclass(X,Y):- subrelation(X,Y).
ontol_db:subclass(X,Y):- is_a(X,Y).

metadata_db:entity_synonym_scope(X,S,exact):- exact_synonym(X,S).
metadata_db:entity_synonym_scope(X,S,broad):- broad_synonym(X,S).
metadata_db:entity_synonym_scope(X,S,narrow):- narrow_synonym(X,S).
metadata_db:entity_synonym_scope(X,S,related):- related_synonym(X,S).

metadata_db:entity_synonym(X,S):- synonym(X,S).

% doesn't work if filtered..
metadata_db:entity_comment(E,CJ):-
        findall(C,comment(E,C),Cs1),
        (   class_instance_relation_pair(E,EI),
            metadata_db:entity_comment(EI,C2)
        ->  Cs2=['INSTANCE LEVEL: ',C2,':: CLASS LEVEL: '|Cs1]
        ;   Cs2=Cs1),
        Cs2 \= [],
        concat_atom(Cs2,CJ).

ontol_db:def(E,X):-
        text_definition(E,D1),
        Ds1=[D1],
        (   class_instance_relation_pair(E,EI),
            ontol_db:def(EI,D2)
        ->  Ds2=['INSTANCE LEVEL: ',D2,':: CLASS LEVEL: '|Ds1]
        ;   Ds2=Ds1),
        Ds2 \= [],
        concat_atom(Ds2,X).
        
