/* -*- Mode: Prolog -*- */

:- module(ontol_manifest_metadata_from_opb,
          []).

:- use_module(bio(ontol_bridge_from_owl2)).
:- use_module(bio(ontol_db),[]).
:- use_module(bio(metadata_db)).
:- use_module(bio(owl_util),[rdf_literal_to_native/2]).
:- use_module(library('thea2/owl2_model')).
:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdfs')).

:- rdf_register_ns('OPB','http://www.owl-ontologies.com/unnamed.owl#').

metadata_db:entity_synonym_scope(C,Syn,exact):-
        entity_synonym(C,Syn).

metadata_db:entity_synonym(C,Syn) :-
        rdfid_oboid(U,C),
        debug(opb,'testing ~w ~w',[C,U]),
        rdfid_oboid(AP, 'OPB:has_Synonym'),
        subClassOf(U,hasValue(AP,Lit)),
        rdf_literal_to_native(Lit,Syn).




