:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).

class_ontcode(C,X):-
        belongs(C,O),
        ont_code(O,X).

ont_code(molecular_function,'MF'):-!.
ont_code(biological_process,'BP'):-!.
ont_code(cellular_component,'CC'):-!.
ont_code(X,X).

f2p(FX-PX,F-FN,P-PN):-
        restriction(F1,executes,P1),
        entity_xref(F1,F),
        entity_xref(P1,P),
        class(F,FN),
        class(P,PN),
        class_ontcode(F,FX),
        class_ontcode(P,PX).

        
