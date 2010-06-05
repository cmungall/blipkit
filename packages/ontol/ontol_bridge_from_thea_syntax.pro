/* -*- Mode: Prolog -*- */


:- module(ontol_bridge_from_thea_syntax,
          [
          ]).

:- use_module(bio('thea/owl_parser')).
:- use_module(bio('thea/owl_as2rdf')).
:- use_module(bio('thea/owl_reasoner')).
:- use_module(bio(ontol_bridge_to_owl),[]).

:- abolish(ontol_bridge_to_owl:owl_assertall/0 ).

% override the goal that generates rdf from ontol_db predicates
ontol_bridge_to_owl:owl_assertall:-
        owl_generate_rdf(no).
