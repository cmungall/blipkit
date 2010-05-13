/* -*- Mode: Prolog -*- */
:- module(ontol_manifest_metadata_from_nif_via_thea,[]).

:- use_module(bio(ontol_db),[]).
:- use_module(bio(metadata_db)).
:- use_module(bio(ontol_bridge_from_owl2)).
:- use_module(library('thea2/owl2_model')).
:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdfs')).
:- use_module(owl_util,[rdf_literal_to_native/2]).
:- use_module(bio(rdf_id_util),[rdfid_oboid/2]).

:- rdf_register_ns('NIF_GrossAnatomy','http://ontology.neuinfo.org/NIF/BiomaterialEntities/NIF-GrossAnatomy.owl#').
:- rdf_register_ns('NIF_Subcellular','http://ontology.neuinfo.org/NIF/BiomaterialEntities/NIF-Subcellular.owl#').
:- rdf_register_ns('NIF_Cell','http://ontology.neuinfo.org/NIF/BiomaterialEntities/NIF-Cell.owl#').
:- rdf_register_ns('NIF_Retired','http://ontology.neuinfo.org/NIF/Retired/NIF-Retired.owl#').
:- rdf_register_ns('NIF_Molecule','http://ontology.neuinfo.org/NIF/BiomaterialEntities/NIF-Molecule.owl#').
:- rdf_register_ns(nif_obo_annot,'http://ontology.neuinfo.org/NIF/Backend/OBO_annotation_properties.owl#').
:- rdf_register_ns(nif_annot,'http://ontology.neuinfo.org/NIF/Backend/BIRNLex_annotation_properties.owl#').
:- rdf_register_ns('NIF_UBO','http://ontology.neuinfo.org/NIF/Backend/BIRNLex-OBO-UBO.owl#').
:- rdf_register_ns('NIF_Investigation','http://ontology.neuinfo.org/NIF/DigitalEntities/NIF-Investigation.owl#').
:- rdf_register_ns('NIF_Quality','http://ontology.neuinfo.org/NIF/BiomaterialEntities/NIF-Quality.owl#').
:- rdf_register_ns('NIF_Dysfunction','http://ontology.neuinfo.org/NIF/Dysfunction/NIF-Dysfunction.owl#').
:- rdf_register_ns('NIF_Organism','http://ontology.neuinfo.org/NIF/BiomaterialEntities/NIF-Organism.owl#').

:- multifile ontol_bridge_from_owl2:suppress_entity/1.
ontol_bridge_from_owl2:suppress_entity('http://ontology.neuinfo.org/NIF/Backend/BIRNLex_annotation_properties.owl#_birnlex_retired_class').

idspace_abbrev('NeuroNames','neuronamesID').
idspace_abbrev('BONFIRE','bonfireID').
idspace_abbrev('UMLS','UmlsCui').
idspace_abbrev('gene_Ontology_ID','GO').
idspace_abbrev('cell_ontology_ID','CL').

:- multifile ontol_bridge_from_owl2:consumed_property/1.
ontol_bridge_from_owl2:consumed_property(P) :-
        idspace_abbrev(_,Abbrev),
        atom_concat('nif_obo_annot:',Abbrev,P).


metadata_db:entity_resource(ID,birnlex_anatomy):-
        ontol_db:class(ID).

metadata_db:entity_xref(ID,Xref):-
	ontol_db:inst_sv(ID,'nif_obo_annot',Xref,_).

metadata_db:entity_xref(ID,Xref):-
	ontol_db:inst_sv(ID,'nif_annot:gene_Ontology_ID',LocalID,_),concat_atom(['GO',LocalID],':',Xref).
metadata_db:entity_xref(ID,Xref):-
	ontol_db:inst_sv(ID,'nif_annot:umls_ID',LocalID,_),concat_atom(['UMLS',LocalID],':',Xref).
metadata_db:entity_xref(ID,Xref):-
	ontol_db:inst_sv(ID,'nif_annot:sao_ID',LocalID,_),concat_atom(['SAO',LocalID],':',Xref).
metadata_db:entity_xref(ID,Xref):-
	ontol_db:inst_sv(ID,'nif_annot:cell_ontology_ID',Xref,_).


% deprecate?
metadata_db:entity_xref(ID,Xref):-
        idspace_abbrev(DB,Abbrev),
	(   AP='nif_obo_annot:'
	;   AP='nif_annot:'),
        atom_concat(AP,Abbrev,P),
        ontol_db:inst_sv(ID,P,Num,_),
        concat_atom([DB,Num],':',Xref),
        Xref\=ID.

metadata_db:entity_synonym_scope(ID,S,exact):-
	metadata_db:entity_synonym(ID,S).

metadata_db:entity_synonym(ID,S):-
        ontol_db:inst_sv(ID,'nif_obo_annot:synonym',S,_).

metadata_db:entity_synonym(ID,S):-
        ontol_db:inst_sv(ID,'nif_obo_annot:abbrev',S,_).

metadata_db:entity_synonym(ID,S):-
        ontol_db:inst_sv(ID,'nif_obo_annot:synonym',S,_).

ontol_db:def(ID,Def):-
        ontol_db:inst_sv(ID,'http://www.w3.org/2004/02/skos/core#definition',Def,_).
ontol_db:def(ID,Def):-
        ontol_db:inst_sv(ID,'nif_annot:birnlexDefinition',Def,_).
ontol_db:def_xref(ID,Xref):-
        ontol_db:inst_sv(ID,'nif_obo_annot:hasDefinitionSource',Xref,_).

metadata_db:entity_obsolete(ID,class):-
        rdfid_oboid(Res,ID),
        %ontol_db:subclass(Res,'nif_annot:_birnlex_retired_class').
	subClassOf(Res,'http://ontology.neuinfo.org/NIF/Backend/BIRNLex_annotation_properties.owl#_birnlex_retired_class').

metadata_db:entity_replaced_by(ID,X):-
        rdfid_oboid(Res,ID),
        ontol_db:inst_sv(Res,'nif_annot:isReplacedByClass',X,_).


%% USAGE:
%% blip -i http://fireball.drexelmed.edu/birnlex/1.2.3/BIRNLex-Anatomy.owl -u ontol_manifest_metadata_from_birnlex io-convert -to obo
% declare annotprops here

%ontol_bridge_from_owl:allowed_inst_sv(A,B,C):- writeln(foo(A-B-C)),fail.
%ontol_bridge_from_owl:allowed_inst_sv(_,nif_obo_annot:neuronamesID,_):- writeln(xxx).
ontol_bridge_from_owl:allowed_inst_sv(_,_,'nif_obo_annot:neuronamesID').





