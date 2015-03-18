:- module(curation_bridge_to_owl2,
	  [
           infer_property_URI/2,
           infer_inverse_property_URI/2,
           rel_label/2
	   ]).

:- use_module(curation_db).
:- use_module(bio(ontol_db)).

:- use_module(bio(ontol_bridge_to_owl2_and_iao)).
:- use_module(bio(ontol_bridge_to_owl2),[uri_oboid/2]).
:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).
:- use_module(library('thea2/owl2_model'),
	      []).

native_to_literal(Term,literal(Term)):- number(Term),!.
native_to_literal(Term,literal(lang(en,Term))).

% ----------------------------------------
% MAPPING
% ----------------------------------------

% each annotation corresponds 1:1 to a GO class *instance*
owl2_model:namedIndividual(UA) :-
        uri_oboid(UA,A),
        curation(A).
owl2_model:classAssertion(GO_Class_URI,Obs_URI) :-
        uri_oboid(Obs_URI,Obs),
        uri_oboid(GO_Class_URI,GO_Class),
        curation_statement(Obs,_,_,GO_Class).

owl2_model:classAssertion( someValuesFrom(Rel_URI,
                                          intersectionOf([ Generic_Mol_Class_URI,
                                                           hasValue(Enc_URI,Gene_URI)])),
                           Obs_URI) :-
        uri_oboid(Obs_URI,Obs),
        uri_oboid(Gene_URI,Gene),
        cl_label(Generic_Mol_Class_URI,'information macromolecule'),
        rel_label(Enc_URI,'encoded by'),
        infer_property_URI(Obs,Rel_URI),
        curation_statement(Obs,Gene,_,_).

owl2_model:annotationAssertion('http://www.w3.org/2000/01/rdf-schema#label',Obs_URI,Literal) :-
        uri_oboid(Obs_URI,Obs),
        curation_statement(Obs,Gene,_,GO),
        entity_label(Gene,Gene_Label),
        entity_label(GO,GO_Label),
        concat_atom([observation,of,GO_Label,involving,Gene_Label,'-',Obs],' ',Obs_Label),
        native_to_literal(Obs_Label,Literal).


% ----------------------------------------
% INFER PROPERTIES
% ----------------------------------------

infer_property_URI(Obs,Rel_URI) :-
        infer_property_name(Obs,Rel_Name),
        rel_label(Rel_URI,Rel_Name).

infer_property_name(Obs,Rel_Name) :-
        curation_statement(Obs,_,_,GO),
        infer_property_name(Obs,GO,Rel_Name).

% note the main ontology need not be loaded, as this is exported
% directly in annotations too

infer_property_name(_Obs,GO,'has participant') :-
        entity_resource(GO,biological_process),
        !.
infer_property_name(_Obs,GO,'has participant') :-
        entity_resource(GO,molecular_function),
        !.
infer_property_name(_Obs,GO,'has part') :-
        entity_resource(GO,cellular_component),
        !.

infer_inverse_property_URI(Obs,InvRel_URI) :-
        infer_property_URI(Obs,Rel_URI),
        owl2_model:inverseProperties(InvRel_URI,Rel_URI).
infer_inverse_property_URI(Obs,InvRel_URI) :-
        infer_property_URI(Obs,Rel_URI),
        owl2_model:inverseProperties(Rel_URI,InvRel_URI).

% ----------------------------------------
% MIREOT from ChEBI
% ----------------------------------------

owl2_model:class(P) :- cl_label(P,_).
owl2_model:annotationAssertion('http://www.w3.org/2000/01/rdf-schema#label',P,literal(lang(en,N))) :- cl_label(P,N).

cl_label('http://purl.obolibrary.org/obo/CHEBI_33695','information macromolecule').

% ----------------------------------------
% MIREOT from RO
% ----------------------------------------

owl2_model:objectProperty(P) :- rel_label(P,_).
owl2_model:annotationAssertion('http://www.w3.org/2000/01/rdf-schema#label',P,literal(lang(en,N))) :- rel_label(P,N).
owl2_model:inverseProperties(P1,P2) :- rel_label(P1,'encoded by'),rel_label(P2,'encodes').
owl2_model:inverseProperties(P1,P2) :- rel_label(P1,'has participant'),rel_label(P2,'participates in').
owl2_model:inverseProperties(P1,P2) :- rel_label(P1,'part of'),rel_label(P2,'has part').


rel_label('http://purl.obolibrary.org/obo/RO_encoded_by','encoded by').
rel_label('http://purl.obolibrary.org/obo/RO_encodes','encodes').
rel_label('http://purl.obolibrary.org/obo/BFO_0000056','participates in').
rel_label('http://purl.obolibrary.org/obo/BFO_0000057','has participant').
rel_label('http://purl.obolibrary.org/obo/BFO_0000050','part of').
rel_label('http://purl.obolibrary.org/obo/BFO_0000051','has part').


/*

  testing:

  blip -r go -ff go_assoc^packages/curation/t/data/gene_assoc.fb.tbl mireot-by-annotations > packages/curation/t/data/go_subset.obo
  blip -i packages/curation/t/data/go_subset.obo -ff go_assoc^packages/curation/t/data/gene_assoc.fb.tbl -u curation_bridge_to_owl2 io-convert -to owl2 -o ~/tmp/gotest.owl

  then try query:

  all processes involving 7SLRNA: 
  'has participant' some ('encoded by' value 7SLRNA)

  [individuals]

    %encodes some ('participates in' some 'protein transport')

  
*/
