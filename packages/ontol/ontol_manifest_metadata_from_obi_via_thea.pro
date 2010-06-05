/* -*- Mode: Prolog -*- */

:- module(ontol_manifest_metadata_from_obi_via_thea,
          []).

:- use_module(bio(ontol_bridge_from_owl2)).
:- use_module(bio(ontol_db),[]).
:- use_module(bio(metadata_db)).
:- use_module(bio(owl_util),[rdf_literal_to_native/2]).
:- use_module(library('thea2/owl2_model')).
:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdfs')).

:- [obo_namespaces].

:- rdf_register_ns(bfo,'http://www.ifomis.org/bfo/1.1#').
:- rdf_register_ns(snap,'http://www.ifomis.org/bfo/1.1/snap#').
:- rdf_register_ns(span,'http://www.ifomis.org/bfo/1.1/span#').

:- rdf_register_ns('OBO_REL','http://www.obofoundry.org/ro/ro.owl#').

:- rdf_register_ns('OBI','http://purl.obolibrary.org/obo/OBI_').
:- rdf_register_ns('IAO','http://purl.obolibrary.org/obo/IAO_').

:- multifile ontol_bridge_from_owl2:suppress_entity/1.
ontol_bridge_from_owl2:suppress_entity('http://www.geneontology.org/formats/oboInOwl#ObsoleteClass').

metadata_db:entity_synonym(X,Syn) :-
        rdfid_oboid(U,X),
        anyPropertyAssertion('http://purl.obolibrary.org/obo/IAO_0000111',U,Lit),
        rdf_literal_to_native(Lit,Syn),
        Syn\='',
        \+ entity_label(X,Syn).
metadata_db:entity_synonym(X,Syn) :-
        rdfid_oboid(U,X),
        anyPropertyAssertion('http://purl.obolibrary.org/obo/IAO_0000118',U,Lit),
        rdf_literal_to_native(Lit,Syn),
        Syn\='',
        \+ entity_label(X,Syn).

metadata_db:entity_synonym_scope(C,Syn,exact):-
        entity_synonym(C,Syn).

ontol_db:def(X,Def):-
        rdfid_oboid(U,X),
        anyPropertyAssertion('http://purl.obolibrary.org/obo/IAO_0000115',U,Lit),
        rdf_literal_to_native(Lit,Def).

metadata_db:entity_obsolete(X,class):-
        rdfid_oboid(U,X),
        subClassOf(U,'http://www.geneontology.org/formats/oboInOwl#ObsoleteClass').

%ontol_db:def_xref(X,Xref):-
%        rdfid_oboid(U,X),
%        anyPropertyAssertion('http://purl.obolibrary.org/obo/IAO_0000119',U,Lit),
%        rdf_literal_to_native(Lit,Xref),
        

