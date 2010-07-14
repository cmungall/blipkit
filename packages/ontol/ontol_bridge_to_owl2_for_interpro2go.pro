/* -*- Mode: Prolog -*- */

:- module(ontol_bridge_to_owl2_for_interpro2go,
          [
           ]).

:- use_module(bio(ontol_bridge_to_owl2_and_iao)).
:- use_module(bio(ontol_bridge_to_owl2),[uri_oboid/2]).
:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).
:- use_module(library('thea2/owl2_model'),
	      []).

native_to_literal(Term,literal(Term)):- number(Term),!.
native_to_literal(Term,literal(lang(en,Term))).

% ----------------------------------------
% REPRESENT IPR AS CLASSES
% ----------------------------------------

owl2_model:class(IPR_URI) :-
        setof(IPR,
              GO^(   entity_xref(GO,IPR),
                     id_idspace(IPR,'InterPro')),
              IPRs),
        member(IPR,IPRs),
        uri_oboid(IPR_URI,IPR).

% ----------------------------------------
% MIREOT PRO + has_part
% ----------------------------------------

cl_label('http://purl.obolibrary.org/obo/PRO_000000001', protein).
pr_label('http://purl.obolibrary.org/obo/BFO_0000051', 'has part').


owl2_model:class(P) :- cl_label(P,_).
owl2_model:objectProperty(P) :- pr_label(P,_).
owl2_model:annotationAssertion('http://www.w3.org/2000/01/rdf-schema#label',P,literal(lang(en,N))) :-
        (   cl_label(P,N)
        ;   pr_label(P,N)).



% ----------------------------------------
% TREAT INTERPRO2GO MAPPING AS CLASSIFICATION GCIs
% ----------------------------------------

% re-root nodes
owl2_model:subClassOf(intersectionOf(['http://purl.obolibrary.org/obo/PRO_000000001',
                                      someValuesFrom('http://purl.obolibrary.org/obo/BFO_0000051',
                                                     IPR_URI)]),
                      GO_URI) :-
	uri_oboid(GO_URI,GO),
	uri_oboid(IPR_URI,IPR),
        entity_xref(GO,IPR),
        id_idspace(GO,'GO'),
        id_idspace(IPR,'InterPro').
