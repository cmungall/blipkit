/* -*- Mode: Prolog -*- */

% UK Ordnance Survey
:- module(ontol_manifest_metadata_from_ukos,
          []).

:- use_module(bio(ontol_bridge_from_owl2)).
:- use_module(bio(ontol_db),[]).
:- use_module(bio(metadata_db)).
:- use_module(bio(owl_util),[rdf_literal_to_native/2]).
:- use_module(library('thea2/owl2_model')).
:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdfs')).

:- rdf_register_ns('ukos_hydrology','http://www.ordnancesurvey.co.uk/ontology/Hydrology/v2.0/Hydrology.owl#').
:- rdf_register_ns('ukos_srel','http://www.ordnancesurvey.co.uk/ontology/SpatialRelations/v0.2/SpatialRelations.owl#').
:- rdf_register_ns('ukos_mrel','http://www.ordnancesurvey.co.uk/ontology/MereologicalRelations/v0.2/MereologicalRelations.owl#').
:- rdf_register_ns('ukos_nrel','http://www.ordnancesurvey.co.uk/ontology/NetworkRelations/v0.2/NetworkRelations.owl#').
:- rdf_register_ns('ukos_rabbit','http://www.ordnancesurvey.co.uk/ontology/Rabbit/v1.0/Rabbit.owl#').
:- rdf_register_ns('ukos_topo','http://www.ordnancesurvey.co.uk/ontology/Topography/v0.1/Topography.owl#').

        

