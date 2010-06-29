
:- module(obol,
          [
           ]).

:- use_module(classbuilder).
:- use_module(obo_grammar).
:- use_module(bio(av_db)).
:- use_module(bio(tokenizer)).

atom_to_cdef(S,C):-
        parse_term(S,T),
        tree_to_class(T,C).


        
