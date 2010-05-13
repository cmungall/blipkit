:- module(homol_bridge_from_ontol,[]).
:- use_module(bio(homol_db),[]).
:- use_module(bio(ontol_db)).

homol_db:homologset_member(Set,Gen,x,y):-
        inst_rel(Gen,'OBO_REL:descended_from',Set).

homol_db:homologous_to(A,B):-
        restriction(A,'OBO_REL:homologous_to',B).
