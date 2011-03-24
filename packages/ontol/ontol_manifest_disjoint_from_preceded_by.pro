:- module(ontol_manifest_disjoint_from_preceded_by,[]).

:- use_module(bio(ontol_db)).

:- multifile ontol_db:disjoint_from/2.
ontol_db:disjoint_from(X,Y):- ontol_db:restriction(X,preceded_by,Y).

