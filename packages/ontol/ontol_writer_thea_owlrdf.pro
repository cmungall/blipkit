/* -*- Mode: Prolog -*- */

:- module(ontol_writer_thea_owlrdf,[]).

:- use_module(bio('thea/owl_as2rdf')).

io:write_all(thea_owlrdf,F,_):-
        owl_generate_rdf(F,_).


