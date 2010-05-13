:- module(ontol_entailment_union,[]).

:- use_module(bio(ontol_db)).

:- multifile ontol_db:subclass/2.
ontol_db:subclass(A,B):- class_union_element(B,A).

