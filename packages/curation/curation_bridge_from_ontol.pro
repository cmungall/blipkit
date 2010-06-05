/* -*- Mode: Prolog -*- */
:- module(curation_bridge_from_ontol,
          []).

:- use_module(bio(curation_db)).
:- use_module(bio(ontol_db)).
:- use_module(bio(bioprolog_util)).

curation_db:curation_statement(C,S,R,O):-
        reification(C,inst_rel(S,R,O)).
curation_db:curation_statement(C,S,R,O):-
        reification(C,restriction(S,R,O)).

