/* -*- Mode: Prolog -*- */

:- module(ontol_manifest_metadata_from_iao,
          []).

:- use_module(bio(ontol_bridge_from_owl2)).
:- use_module(bio(ontol_db),[]).
:- use_module(bio(metadata_db)).
:- use_module(bio(owl_util),[rdf_literal_to_native/2]).
:- use_module(library('thea2/owl2_model')).
:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdfs')).

:- rdf_register_ns(bfo,'http://www.ifomis.org/bfo/1.1#').
:- rdf_register_ns(snap,'http://www.ifomis.org/bfo/1.1/snap#').
:- rdf_register_ns(span,'http://www.ifomis.org/bfo/1.1/span#').

:- rdf_register_ns('OBO_REL','http://www.obofoundry.org/ro/ro.owl#').

:- rdf_register_ns('OBI','http://purl.obolibrary.org/obo/OBI_').
:- rdf_register_ns('IAO','http://purl.obolibrary.org/obo/IAO_').
:- rdf_register_ns('OGMS','http://purl.obolibrary.org/obo/OGMS_').
:- rdf_register_ns('IDO','http://purl.obolibrary.org/obo/IDO_').

:- multifile ontol_bridge_from_owl2:suppress_entity/1.
ontol_bridge_from_owl2:suppress_entity('http://www.geneontology.org/formats/oboInOwl#ObsoleteClass').

metadata_db:entity_resource(X,R) :-
        ontol_db:class(X),
        id_idspace(X,R).
metadata_db:entity_resource(X,R) :-
        ontol_db:property(X),
        id_idspace(X,R).

metadata_db:entity_synonym(X,Syn) :-
        uri_oboid(U,X),
        anyPropertyAssertion('http://purl.obolibrary.org/obo/IAO_0000111',U,Lit),
        %inst_sv(U,'IAO:0000111',Lit),
        rdf_literal_to_native(Lit,Syn),
        Syn\='',
        \+ entity_label(X,Syn).
metadata_db:entity_synonym(X,Syn) :-
        uri_oboid(U,X),
        anyPropertyAssertion('http://purl.obolibrary.org/obo/IAO_0000118',U,Lit),
        %anyPropertyAssertion('IAO:0000118',U,Lit),
        rdf_literal_to_native(Lit,Syn),
        Syn\='',
        \+ entity_label(X,Syn).

metadata_db:entity_synonym_scope(C,Syn,exact):-
        entity_synonym(C,Syn).

ontol_db:def(X,Def):-
        uri_oboid(U,X),
        anyPropertyAssertion('http://purl.obolibrary.org/obo/IAO_0000115',U,Lit),
        %anyPropertyAssertion('IAO:0000115',U,Lit),
        rdf_literal_to_native(Lit,Def).

ontol_db:expand_expression_to(X,Def):-
        uri_oboid(U,X),
        anyPropertyAssertion('http://purl.obolibrary.org/obo/IAO_0000424',U,Lit),
        rdf_literal_to_native(Lit,Def).

ontol_db:expand_assertion_to(X,Def):-
        uri_oboid(U,X),
        anyPropertyAssertion('http://purl.obolibrary.org/obo/IAO_0000425',U,Lit),
        rdf_literal_to_native(Lit,Def).

% note that FLU and others continue to use the oboinowl APs
metadata_db:entity_obsolete(X,class):-
	ontol_db:class(X),
        uri_oboid(U,X),
        subClassOf(U,'http://www.geneontology.org/formats/oboInOwl#ObsoleteClass').
%        subClassOf(U,'oboInOwl:ObsoleteClass').


