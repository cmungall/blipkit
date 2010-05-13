:- module(ontol_entailment_basic,[]).

:- use_module(bio(ontol_db)).

ontol_db:disjoint_from(X,Y):- ontol_db:restriction(X,preceded_by,Y).

