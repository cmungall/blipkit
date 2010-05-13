/* -*- Mode: Prolog -*- */
:- module(ontol_manifest_metadata_from_oboInOwl,[]).

:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).
:- use_module(bio(tabling)).
:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdfs')).
:- use_module(library('semweb/rdfs')).

idspace_abbrev('SAO',sao_ID).
idspace_abbrev('UMLS',umls_ID).
idspace_abbrev('BONFIRE',bonfire_ID).
%idspace_abbrev('CL','Cell_ontology_ID').
idspace_abbrev('CL','cell_ontology_ID').
idspace_abbrev('GO','gene_Ontology_ID').

:- multifile ontol_bridge_from_owl:reserved_property/1.
ontol_bridge_from_owl:reserved_property(P):-
	debug(ontol,'testing for suppression of ~w',[P]),
	rdf_split_url('http://www.geneontology.org/formats/oboInOwl#',_,P).

%ontol_bridge_from_owl:reserved_property(sao:abbreviation).
%ontol_bridge_from_owl:reserved_property(sao:definition).
%ontol_bridge_from_owl:reserved_property(sao:P):- idspace_abbrev(_,P).

:- rdf_register_ns(oboInOwl,'http://www.geneontology.org/formats/oboInOwl#',[force(true)]).

:- multifile ontol_bridge_from_owl:suppress_resource/1.
ontol_bridge_from_owl:suppress_resource(X):-
	rdfs_individual_of(X,Type),
	rdf_split_url('http://www.geneontology.org/formats/oboInOwl#',_,Type).

% add prefix or not?
metadata_db:entity_xref(ID,Xref):-
        idspace_abbrev(DB,Abbrev),
        atom_concat('sao:',Abbrev,P),
        inst_sv(ID,P,Num,_),
        concat_atom([DB,Num],':',Xref),
        Xref\=ID.

metadata_db:entity_synonym(ID,S):-
        inst_sv(ID,'sao:synonym',S,_).

metadata_db:entity_synonym(ID,S):-
        inst_sv(ID,'sao:abbreviation',S,_).

ontol_db:def(ID,Def):-
        inst_sv(ID,'sao:definition',Def,_).
ontol_db:def_xref(ID,Xref):-
        inst_sv(ID,'birn_annot:birnlexDefinitionSource',Xref,_).


:- table_pred(ontol_db:subclassRT/2).

% this is very slow unless we table
metadata_db:entity_obsolete(ID,class):-
        entity_label(RID,'Obsolete class'),
        subclassRT(ID,RID).

%% USAGE:
%% blip -i 'http://ccdb.ucsd.edu/SAO/1.2/SAO.owl -u ontol_manifest_metadata_from_sao io-convert -to obo
