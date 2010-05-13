:- module(ontol_entailment_genus_differentia,[]).

:- use_module(bio(ontol_db)).
%:- use_module(bio(ontol_entailment_basic)).

%:- multifile ontol_db:subclass/2.
ontol_db:subclass(C1,C2):-
        genus(C2,G2),
        forall(differentium(C2,R,D2),
               (   restriction(C1,R,D1),
                   subclassRT(D1,D2))),
        subclassRT(C1,G2).


