/* -*- Mode: Prolog -*- */
:- module(curation_bridge_to_ontol_inverted,
          [
          ]).

:- use_module(bio(curation_db)).
:- use_module(bio(ontol_db)).
:- use_module(bio(seqfeature_db)).
:- use_module(bio(bioprolog_util)).

ontol_db:class(X) :- feature(X).

ontol_db:restriction(Term,has,Gene):-
        curation_statement(_,Gene,_,Term).

        
