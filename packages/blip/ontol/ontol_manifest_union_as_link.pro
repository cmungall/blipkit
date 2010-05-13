:- module(ontol_manifest_union_as_link,[]).

:- use_module(bio(ontol_db)).

:- multifile ontol_db:restriction/3.
ontol_db:restriction(A,is_a_u,B):- class_union_element(B,A).

