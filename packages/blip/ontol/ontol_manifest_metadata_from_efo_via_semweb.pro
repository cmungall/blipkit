/* -*- Mode: Prolog -*- */

:- module(ontol_manifest_metadata_from_efo_via_semweb,
          []).

:- use_module(bio(ontol_bridge_from_owl)).
:- use_module(bio(ontol_db),[]).
:- use_module(bio(metadata_db)).
:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdfs')).
:- use_module(owl_util,[rdf_literal_to_native/2]).

:- rdf_register_ns('EFO','http://www.ebi.ac.uk/efo/').
:- multifile ontol_bridge_from_owl:allowed_inst_sv/3.

:- multifile ontol_bridge_from_owl:suppress_resource/1.

ontol_bridge_from_owl:suppress_resource(X):- rdf_is_bnode(X).

metadata_db:entity_synonym(X,Syn) :-
        rdfid_oboid(U,X),
        inst_sv(U,'EFO:efo.owl#alternative_term',Syn,_),
        Syn\=''.
ontol_bridge_from_owl:allowed_inst_sv(_,_,'EFO:efo.owl#alternative_term').

metadata_db:entity_synonym_scope(C,Syn,exact):-
        entity_synonym(C,Syn).

ontol_db:def(X,Def):-
        rdfid_oboid(U,X),
        inst_sv(U,'EFO:efo.owl#definition',Def,_).
ontol_bridge_from_owl:allowed_inst_sv(_,_,'EFO:efo.owl#definition').


ontol_db:def_xref(ID,Xref):-
        ontol_db:inst_sv(ID,'EFO:efo.owl#definition_citation',Xref,_).
ontol_bridge_from_owl:allowed_inst_sv(_,_,'EFO:efo.owl#definition_citation').

%metadata_db:entity_obsolete(X,class):-
%       rdfid_oboid(U,X),
%        subClassOf(U,'http://www.geneontology.org/formats/oboInOwl#ObsoleteClass').
        

