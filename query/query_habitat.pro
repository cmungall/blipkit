:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).

metadata_db:entity_xref(ID,X):-
        match(_,_,X,ID,_,_,_,_).

