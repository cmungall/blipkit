/* -*- Mode: Prolog -*- */



:- module(ontol_owlmap_from_obi,
          []).

:- use_module(bio(ontol_bridge_from_owl)).
:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).
:- use_module(bio(mode)).
:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdfs')).

:- multifile ontol_bridge_from_owl:reserved_property/1.
ontol_bridge_from_owl:reserved_property(obi:definition).
ontol_bridge_from_owl:reserved_property('obi:definition').
ontol_bridge_from_owl:reserved_property('obi:branch').

metadata_db:entity_resource(C,Ont):-
        rdfid_oboid(C_RDF,C),
        rdf_has(C_RDF,obi:branch,Lit),
        literal_to_native(Lit,Ont).
ontol_db:def(C,Def):-
        rdfid_oboid(Res,C),
        rdf_has(Res,obi:definition,Lit),
        literal_to_native(Lit,Def).
