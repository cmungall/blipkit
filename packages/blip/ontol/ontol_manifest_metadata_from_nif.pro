/* -*- Mode: Prolog -*- */
:- module(ontol_manifest_metadata_from_nif,[]).

:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).
:- use_module(bio(ontol_bridge_from_owl)).
:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdfs')).
:- use_module(owl_util,[rdf_literal_to_native/2]).

:- rdf_register_ns('NIF_GrossAnatomy','http://ontology.neuinfo.org/NIF/BiomaterialEntities/NIF-GrossAnatomy.owl#').
:- rdf_register_ns('NIF_Subcellular','http://ontology.neuinfo.org/NIF/BiomaterialEntities/NIF-Subcellular.owl#').
:- rdf_register_ns('NIF_Cell','http://ontology.neuinfo.org/NIF/BiomaterialEntities/NIF-Cell.owl#').
:- rdf_register_ns('NIF_Retired','http://ontology.neuinfo.org/NIF/Retired/NIF-Retired.owl#').
:- rdf_register_ns('NIF_Molecule','http://ontology.neuinfo.org/NIF/BiomaterialEntities/NIF-Molecule.owl#').
%:- rdf_register_ns(nif_anatomy,'http://ontology.neuinfo.org/NIF/BiomaterialEntities/NIF-GrossAnatomy.owl#').
%:- rdf_register_ns(nif_subcellular,'http://ontology.neuinfo.org/NIF/BiomaterialEntities/NIF-Subcellular.owl#').
%:- rdf_register_ns(nif_cell,'http://ontology.neuinfo.org/NIF/BiomaterialEntities/NIF-Cell.owl#').
%:- rdf_register_ns(nif_retired,'http://ontology.neuinfo.org/NIF/Retired/NIF-Retired.owl#').
%:- rdf_register_ns(nif_molecule,'http://ontology.neuinfo.org/NIF/BiomaterialEntities/NIF-Molecule.owl#').
:- rdf_register_ns(nif_annot,'http://ontology.neuinfo.org/NIF/Backend/OBO_annotation_properties.owl#').
:- rdf_register_ns(nif_annot,'http://ontology.neuinfo.org/NIF/Backend/OBO_annotation_properties.owl#').
:- rdf_register_ns('PKB','http://ccdb.ucsd.edu/SAO/DPO/2.0/DPO.owl#').

:- multifile ontol_bridge_from_owl:allowed_inst_sv/3.

:- multifile ontol_bridge_from_owl:suppress_resource/1.
ontol_bridge_from_owl:suppress_resource('http://ontology.neuinfo.org/NIF/Backend/BIRNLex_annotation_properties.owl#_birnlex_retired_class').


idspace_abbrev('NeuroNames','neuronamesID').
idspace_abbrev('BONFIRE','bonfireID').
idspace_abbrev('UMLS','UmlsCui').

metadata_db:entity_resource(ID,birnlex_anatomy):-
        class(ID).

metadata_db:entity_xref(ID,Xref):-
	inst_sv(ID,'nif_annot',Xref,_).

metadata_db:entity_xref(ID,Xref):-
        idspace_abbrev(DB,Abbrev),
        atom_concat('nif_annot:',Abbrev,P),
        inst_sv(ID,P,Num,_),
        concat_atom([DB,Num],':',Xref),
        Xref\=ID.
ontol_bridge_from_owl:allowed_inst_sv(_,_,P):- 
        idspace_abbrev(_,Abbrev),
        atom_concat('nif_annot:',Abbrev,P).

metadata_db:entity_synonym(ID,S):-
        inst_sv(ID,'nif_annot:synonym',S,_).
ontol_bridge_from_owl:allowed_inst_sv(_,_,'nif_annot:synonym').

metadata_db:entity_synonym(ID,S):-
        inst_sv(ID,'nif_annot:abbrev',S,_).

metadata_db:entity_synonym(ID,S):-
        inst_sv(ID,'nif_annot:synonym',S,_).

ontol_db:def(ID,Def):-
        inst_sv(ID,'http://www.w3.org/2004/02/skos/core#definition',Def,_).
ontol_db:def(ID,Def):-
        inst_sv(ID,'nif_annot:birnlexDefinition',Def,_).
ontol_db:def_xref(ID,Xref):-
        inst_sv(ID,'nif_annot:hasDefinitionSource',Xref,_).
ontol_bridge_from_owl:allowed_inst_sv(_,_,'nif_annot:birnlexDefinition').
ontol_bridge_from_owl:allowed_inst_sv(_,_,'nif_annot:hasBirnlexDefinitionSource2').

metadata_db:entity_obsolete(ID,class):-
        rdfid_oboid(Res,ID),
        rdf_has(Res,rdfs:subClassOf,'http://ontology.neuinfo.org/NIF/Backend/BIRNLex_annotation_properties.owl#_birnlex_retired_class').

metadata_db:entity_replaced_by(ID,X):-
        rdfid_oboid(Res,ID),
        rdf_has(Res,'http://ontology.neuinfo.org/NIF/Backend/BIRNLex_annotation_properties.owl#isReplacedByClass',Lit),
        rdf_literal_to_native(Lit,X).


%% USAGE:
%% blip -i http://fireball.drexelmed.edu/birnlex/1.2.3/BIRNLex-Anatomy.owl -u ontol_manifest_metadata_from_birnlex io-convert -to obo
% declare annotprops here

%ontol_bridge_from_owl:allowed_inst_sv(A,B,C):- writeln(foo(A-B-C)),fail.
%ontol_bridge_from_owl:allowed_inst_sv(_,nif_annot:neuronamesID,_):- writeln(xxx).
ontol_bridge_from_owl:allowed_inst_sv(_,_,'nif_annot:neuronamesID').





