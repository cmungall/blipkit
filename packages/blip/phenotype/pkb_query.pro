
:- use_module(phenotype_db).
:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).


feature_nifp(F,(E2,Q,D2,W2)) :-
        feature_phenotype(F,P),
        P=(E,Q,D,W),
        mapid(E,E2),
        mapid(D,D2),
        mapid(W,W2).

mapid(-,-).
mapid(thing,thing).
mapid(A,B) :-
        entity_xref(U,A),
        entity_xref(U,B),
        class(B),
        sub_atom(B,0,_,_,'NIF').
        
        
        
