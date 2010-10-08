/* -*- Mode: Prolog -*- */

:- module(pathway_writer_gaf,
          [
           ]).

:- use_module(bio(metadata_db)).
:- use_module(bio(pathway_db)).
:- use_module(bio(interaction_db)).
:- use_module(bio(curation_db)).
:- use_module(bio(bioprolog_util)).
:- [adhoc_reactome].            % TODO

gaf(GafTerm) :-
        