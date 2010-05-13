:- use_module(bio(ontol_db)).

ontol_db:restriction(F,realized_by,P):-
        class_xref(F,X),
        belongs(F,molecular_function),
        class_xref(P,X),
        belongs(P,biological_process).
