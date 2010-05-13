/* -*- Mode: Prolog -*- */

:- module(obolog_filter_bfo,[]).
:- use_module(bio(parser_obolog)).
:- use_module(bio(io)).

:- multifile io:filter_fact/2.
io:filter_fact(domain(_,_),_).
io:filter_fact(range(_,_),_).
