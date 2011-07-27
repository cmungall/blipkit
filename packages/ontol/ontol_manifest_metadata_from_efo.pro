/* -*- Mode: Prolog -*- */

:- module(ontol_manifest_metadata_from_efo,
          []).

:- use_module(bio(ontol_bridge_from_owl2)).
:- use_module(bio(ontol_db),[]).
:- use_module(bio(metadata_db)).
:- use_module(bio(owl_util),[rdf_literal_to_native/2]).
:- use_module(bio(rdf_id_util),[rdfid_oboid/2]).
:- use_module(library('thea2/owl2_model')).
:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdfs')).

:- rdf_register_ns('EFO','http://www.ebi.ac.uk/efo/EFO_').
:- rdf_register_ns('CHEBI','http://www.ebi.ac.uk/chebi/searchId.do;?chebiId=CHEBI:').
:- rdf_register_ns('OBI','http://purl.obolibrary.org/obo/OBI_').

:- multifile ontol_bridge_from_owl2:suppress_entity/1.
ontol_bridge_from_owl2:suppress_entity('http://www.geneontology.org/formats/oboInOwl#ObsoleteClass').

metadata_db:entity_synonym(X,Syn) :-
        rdfid_oboid(U,X),
        anyPropertyAssertion('http://www.ebi.ac.uk/efo/alternative_term',U,Lit),
        rdf_literal_to_native(Lit,Syn),
        Syn\=''.

metadata_db:entity_synonym_scope(C,Syn,exact):-
        entity_synonym(C,Syn).

ontol_db:def(X,Def):-
        rdfid_oboid(U,X),
        anyPropertyAssertion('http://www.ebi.ac.uk/efo/definition',U,Lit),
        rdf_literal_to_native(Lit,Def).

ontol_db:def_xref(ID,Xref):-
        ontol_db:inst_sv(ID,'http://www.ebi.ac.uk/efo/definition_citation',Xref,_).

%metadata_db:entity_obsolete(X,class):-
%       rdfid_oboid(U,X),
%        subClassOf(U,'http://www.geneontology.org/formats/oboInOwl#ObsoleteClass').
        

