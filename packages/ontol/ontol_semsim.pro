:- module(ontol_semsim,
          [class_pair_simj/3]).


:- use_module(bio(simmatrix)).
:- use_module(ontol_db).

:- dynamic prepared/0.

prepare :-
        prepared,!.

prepare :-
        !,
        generate_term_indexes(C,P,(class(C),entity_label(C,_),debug(sim,'testing ~w',[C]),bf_parentRT(C,P))),
        assert(prepared).

%% class_pair_simj(?A,?B,?S)
class_pair_simj(A,B,S) :-
        prepare,
        feature_pair_simj(A,B,S).


        

