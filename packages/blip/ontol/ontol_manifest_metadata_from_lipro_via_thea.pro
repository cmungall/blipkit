/* -*- Mode: Prolog -*- */

:- module(ontol_manifest_metadata_from_lipro_via_thea,
          []).

:- use_module(bio(ontol_bridge_from_owl2_ext)). % class expressions
:- use_module(bio(ontol_db),[]).
:- use_module(bio(metadata_db)).
:- use_module(bio(owl_util),[rdf_literal_to_native/2]).
:- use_module(library('thea2/owl2_model')).
:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdfs')).

:- rdf_register_ns('LIPRO','http://NUS.I2R.lipidontology.biochem.nus.edu.sg/lipidversion3.owl#').

