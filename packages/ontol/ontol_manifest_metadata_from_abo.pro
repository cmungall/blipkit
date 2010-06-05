/* -*- Mode: Prolog -*- */
:- module(ontol_manifest_metadata_from_abo,[]).

:- use_module(bio(ontol_db),[]).
:- use_module(bio(metadata_db)).
:- use_module(bio(ontol_bridge_from_owl2)).
:- use_module(library('thea2/owl2_model')).

:- rdf_register_ns('ABO','http://www.owl-ontologies.com/unnamed.owl#').

:- multifile ontol_bridge_from_owl2:suppress_entity/1.
ontol_bridge_from_owl2:suppress_entity('http://www.owl-ontologies.com/unnamed.owl#ABO_TERM').


ontol_db:class(ID):-
        ontol_db:inst_of(ID,'http://www.owl-ontologies.com/unnamed.owl#ABO_TERM').





