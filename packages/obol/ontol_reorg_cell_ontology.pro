/* -*- Mode: Prolog -*- */
:- module(ontol_reorg_cell_ontology,
          [
           write_obo/0,
           cell_by_function/1,
           cell_by_function_root/1,
           cell_by_function_subroot/1,
           cell_by_function_only/1
           ]).

:- use_module(bio(bioprolog_util)).
:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).

cell_by_function(X):- class(R,'cell by function'),subclassT(X,R).
cell_by_function_root(X):- class(R,'cell by function'),subclass(X,R).
cell_by_function_subroot(X):- class(R,'cell by function'),subclass(Z,R),subclass(X,Z).
cell_by_function_only(X):- cell_by_function(X),\+ cell_by_non_function(X).

cell_by_non_function(X):- class(R1,'cell by function'),subclass(R1,R),subclass(R2,R),R1\=R2,subclassT(X,R2).

function(F,FN,C):-
        cell_by_function_root(C),
        entity_label(C,N),
        atom_concat(N,' function',FN),
        concat_atom([_,ID],':',C),
        concat_atom(['CJM_FUNC',ID],':',F).

write_obo:-
        forall(function(F,FN,C),
               write_obo(F,FN,C)).

write_obo(F,FN,C):-
        class(Genus,cell),
        class(C,CN),
        format('[Term]~nid: ~w~nname: ~w~nnamespace: cell_function~n~n',[F,FN]),
        format('[Term]~nid: ~w ! ~w~nintersection_of: ~w ! cell~nintersection_of: has_function ~w ! ~w~n~n',[C,CN,Genus,F,FN]),
        nl.
        

/*
ontol_db:class(F):- function(F,_,_).
metadata_db:entity_label(F,N):- function(F,N,_).
ontol_db:subclass(F,'CJM_FUNC:0000001'):- function(F,_,_).
metadata_db:entity_resource(F,cell_function):- function(F,_,_).

ontol_db:genus(C,Genus):- function(_,_,C),class(Genus,'cell').
ontol_db:differentium(C,has_function,F):- function(F,_,C).

*/
