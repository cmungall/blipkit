:- module(ontol_entailment_union,[]).

:- use_module(bio(ontol_db)).

:- multifile ontol_db:subclass/2.
ontol_db:subclass(A,B):- class_union_element(B,A).
ontol_db:class(A):- class_union_element(_,A).

new_subclass(U,X):-
        class_union_element(U,E1),
        subclassT(E1,X),
        forall(class_union_element(U,E),
               subclassT(E,X)),
        \+ ((subclassT(E1,X2),
             subclassT(X2,X),
             forall(class_union_element(U,E),
                    subclassT(E,X2)))).

             


