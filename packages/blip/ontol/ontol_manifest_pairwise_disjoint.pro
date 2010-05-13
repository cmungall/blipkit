:- module(ontol_manifest_pairwise_disjoint,[]).

:- use_module(bio(ontol_db)).

ontol_db:disjoint_from(X,Y):-
        subclass(X,P),
        %all_direct_subclasses_disjoint(P),
        subclass(Y,P).

