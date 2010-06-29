:- module(hook_hier,[]).

:- use_module(bio(bioprolog_util)).

:- multifile classdef_parser:id_cdef_fitness_hook/3.

classdef_parser:id_cdef_fitness_hook(ID,cdef(G,Diffs),S) :-
        solutions(R=D,
                  (   member(R=D,Diffs),
                      t(R,ID,D)),
                  L1),
        length(L1,S1),
        (   subclassT(ID,G)
        ->  S is S1+1.5
        ;   S=S1).

t(R,ID,D) :-
        entity_xref(R2,R),
        t(R2,ID,D),
        !.
t(R,ID,D) :-
        restriction(ID,R,D),!.
t(R,ID,D) :-
        parent_overT(R,ID,D),!.
t(R,ID,D) :-
        (   inverse_of_on_instance_level(R,R2)
        ;   inverse_of_on_instance_level(R2,R)),
        parent_overT(R2,D,ID),!.


