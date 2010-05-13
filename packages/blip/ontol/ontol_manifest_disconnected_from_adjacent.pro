:- module(ontol_entailment_basic,[]).

:- use_module(bio(ontol_db)).

ontol_db:restriction(X,disconnected_from,Y):- ontol_db:restriction(X,adjacent_to,Y).

