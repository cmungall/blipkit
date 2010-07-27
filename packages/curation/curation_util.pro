:- module(curation_util,
          [mireot_by_annotation/1]).


:- use_module(curation_db).
:- use_module(bio(ontol_db)).

mireot_by_annotation(ID) :-
        setof(X,A^G^R^curation_statement(A,G,R,X),Xs),
        bf_set_parentRT(Xs,ID).

