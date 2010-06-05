/* -*- Mode: Prolog -*- */
:- module(ontol_manifest_metadata_from_birn,[]).

:- use_module(bio(ontol_manifest_names_from_ids)).
:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).
:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdfs')).

:- rdf_register_ns(sao,'http://ccdb.ucsd.edu/SAO/1.2#').
:- rdf_register_ns(,'http://ccdb.ucsd.edu/PDPhenotypeOntology/1.0#').

idspace_abbrev('SAO',sao_ID).
idspace_abbrev('UMLS',umls_ID).
idspace_abbrev('UMLS',gene_Ontology_ID).
idspace_abbrev('BONFIRE',bonfire_ID).
idspace_abbrev('CL','Cell_ontology_ID').
idspace_abbrev('GO','gene_Ontology_ID').

% hide certain properties in owlmap
% DOESN'T WORK!! they are needed to get props in the first place
%:- multifile ontol_bridge_from_owl:reserved_property/1.
%ontol_bridge_from_owl:reserved_property(sao:synonym).
%ontol_bridge_from_owl:reserved_property(sao:abbreviation).
%ontol_bridge_from_owl:reserved_property(sao:definition).
%ontol_bridge_from_owl:reserved_property(sao:P):- idspace_abbrev(_,P).

metadata_db:entity_xref(ID,Xref):-
        idspace_abbrev(DB,Abbrev),
        atom_concat('sao:',Abbrev,P),
        inst_sv(ID,P,Num,_),
        concat_atom([DB,Num],':',Xref).

metadata_db:entity_synonym(ID,S):-
        inst_sv(ID,'sao:synonym',S,_).

metadata_db:entity_synonym(ID,S):-
        inst_sv(ID,'sao:abbreviation',S,_).

ontol_db:def(ID,Def):-
        inst_sv(ID,'sao:definition',Def,_).
