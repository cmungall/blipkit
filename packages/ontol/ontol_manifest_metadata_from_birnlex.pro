/* -*- Mode: Prolog -*- */
:- module(ontol_manifest_metadata_from_birnlex,[]).

:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).
:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdfs')).


:- rdf_register_ns(birnlex_main,'http://www.nbirn.net/birnlex/1.2.3/BIRNLex-Main.owl#').
:- rdf_register_ns(birnlex_anatomy,'http://purl.org/nbirn/birnlex/ontology/BIRNLex-Anatomy.owl#').
:- rdf_register_ns(birn_annot,'http://purl.org/nbirn/birnlex/ontology/annotation/OBO_annotation_properties.owl#').
%:- rdf_register_ns(birn_annot,'http://purl.org/nbirn/birnlex/1.2.3/BIRNLex_annotation_properties.owl#').
%                              'http://purl.org/nbirn/birnlex/1.2.3/BIRNLex_annotation_properties.owl#birnlexDefinition'

:- multifile ontol_bridge_from_owl:allowed_inst_sv/3.

idspace_abbrev('NeuroNames','neuronamesID').
idspace_abbrev('BONFIRE','bonfireID').
idspace_abbrev('UMLS','UmlsCui').

metadata_db:entity_resource(ID,birnlex_anatomy):-
        class(ID).

metadata_db:entity_xref(ID,Xref):-
        idspace_abbrev(DB,Abbrev),
        atom_concat('birn_annot:',Abbrev,P),
        inst_sv(ID,P,Num,_),
        concat_atom([DB,Num],':',Xref),
        Xref\=ID.
ontol_bridge_from_owl:allowed_inst_sv(_,_,P):- 
        idspace_abbrev(_,Abbrev),
        atom_concat('birn_annot:',Abbrev,P).


metadata_db:entity_synonym(ID,S):-
        inst_sv(ID,'birn_annot:synonym',S,_).
ontol_bridge_from_owl:allowed_inst_sv(_,_,'birn_annot:synonym').

metadata_db:entity_synonym(ID,S):-
        inst_sv(ID,'birn_annot:abbrev',S,_).

metadata_db:entity_synonym(ID,S):-
        inst_sv(ID,'birn_annot:synonym',S,_).

ontol_db:def(ID,Def):-
        inst_sv(ID,'birn_annot:birnlexDefinition',Def,_).
ontol_db:def_xref(ID,Xref):-
        inst_sv(ID,'birn_annot:hasBirnlexDefinitionSource2',Xref,_).
ontol_bridge_from_owl:allowed_inst_sv(_,_,'birn_annot:birnlexDefinition').
ontol_bridge_from_owl:allowed_inst_sv(_,_,'birn_annot:hasBirnlexDefinitionSource2').

metadata_db:entity_obsolete(ID,class):- subclass(ID,'birn_annot:_birnlex_retired_class').

%% USAGE:
%% blip -i http://fireball.drexelmed.edu/birnlex/1.2.3/BIRNLex-Anatomy.owl -u ontol_manifest_metadata_from_birnlex io-convert -to obo
% declare annotprops here

%ontol_bridge_from_owl:allowed_inst_sv(A,B,C):- writeln(foo(A-B-C)),fail.
%ontol_bridge_from_owl:allowed_inst_sv(_,birn_annot:neuronamesID,_):- writeln(xxx).
ontol_bridge_from_owl:allowed_inst_sv(_,_,'birn_annot:neuronamesID').





