/* -*- Mode: Prolog -*- */

:- module(obolog_filter_type_type,[]).
:- use_module(bio(parser_obolog)).
:- use_module(bio(io)).

:- multifile io:filter_fact/2.
io:filter_fact(Fact,Facts):-
        Fact=..[_,ID|_],
        member(type_type(ID),Facts).



        
