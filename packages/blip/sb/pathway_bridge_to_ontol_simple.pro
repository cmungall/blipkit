:- module(pathway_bridge_to_ontol_simple,[]).

:- use_module(bio(pathway_db)).
:- use_module(bio(seqfeature_db)).
:- use_module(bio(curation_db)).
:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).

curation_db:curation_statement(A,Protein,Role,Process) :-
        event_participant_role(Process,Participant,Role),
        has_leaf_part(Participant,MolSnap),
        is_protein(MolSnap),
        snapshot_continuant(MolSnap,C),
        entity_db_id(C,'UniProt',Protein),
        A=Protein-Role-Process.

seqfeature_db:feature(P) :-
        is_protein(MolSnap),
        snapshot_continuant(MolSnap,C),
        entity_db_id(C,'UniProt',P).

metadata_db:entity_synonym(P,N) :-
        is_protein(MolSnap),
        snapshot_continuant(MolSnap,C),
        entity_db_id(C,'UniProt',P),
        entity_label(C,N).

ontol_db:class(P) :-
        event(P).

ontol_db:restriction(A,part_of,B) :- subpathway_of(A,B).


/** <module> bridges between the native blip prolog pathway model and a generic ontology model

---+ Synopsis

---+ Details

Useful e.g. for adding to term enrichment results

---+ Additional Information

This module is part of blip. For more details, see http://www.blipkit.org

@author  Chris Mungall
@version $Revision$
@see     README
@license License


*/
