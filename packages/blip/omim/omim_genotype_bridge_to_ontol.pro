/* -*- Mode: Prolog -*- */


:- module(omim_genotype_bridge_to_ontol,[]).
:- use_module(bio(aminoacid_chebi)).
:- use_module(bio(omim_db)).
:- use_module(bio(ontol_db)).

prepend_prefix(ID,OboID):-
        concat_atom([A,B],'-',ID),
        !,
        concat_atom([A,B],'.',ID2),
        prepend_prefix(ID2,OboID).
prepend_prefix(ID,OboID):-
        atom_concat('MIM:',ID,OboID).

ontol_db:ontology(omim,omim,'Omim mapped to classes').

% mutations are instances of omim diseases
ontol_db:class(MutIDFull):-
        omim_mutation(MutID,_,_),
        prepend_prefix(MutID,MutIDFull).
metadata_db:entity_label(MutIDFull,Desc):-
        omim_mutation(MutID,_,Desc),
        prepend_prefix(MutID,MutIDFull).
%ontol_db:inst_of(MutIDFull,'SO:0001027'):-
%        omim_mutation(MutID,_,_),
%        prepend_prefix(MutID,MutIDFull).
ontol_db:restriction(MutIDFull,associated_with,OmimIDFull):-
        omim_mutation(MutID,OmimID,_),
        prepend_prefix(MutID,MutIDFull),
        prepend_prefix(OmimID,OmimIDFull).
metadata_db:entity_comment(MutIDFull,Text):-
        omim_mutation_text(MutID,Text),
        prepend_prefix(MutID,MutIDFull).

