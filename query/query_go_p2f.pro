:- use_module(bio(ontol_db)).

ontol_db:restriction(P,realizes,F):-
        class_xref(P,X),
        belongs(P,biological_process),
        class_xref(F,X),
        belongs(F,molecular_function).

