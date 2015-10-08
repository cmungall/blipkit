:- use_module(bio(metadata_db)).
:- use_module(bio(ontol_db)).

:- multifile ontol_db:parent/3.
ontol_db:parent(X,R,Y) :- gci_restriction(X,R,Y,_,_).

