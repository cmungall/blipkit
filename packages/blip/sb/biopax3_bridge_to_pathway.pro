:- module(biopax3_bridge_to_ontol,[]).

:- use_module(bio(biopax3_db)).
:- use_module(bio(biopax3_bridge_from_rdf)).
:- use_module(bio(metadata_db)).
:- use_module(bio(pathway_db),[]).

:- [biopax_idspace_replace_rules].

% ---- mapping of xrefs ----
% BioPax has an unusual pattern which looks like this:
% 
% 4D-entity --[entityReference]--> EntityReference --[{xref,name}]--> Xref --[{db,id}]--> string
% 
% the domain of entityReference is dna(region) or protein or rna(region) or smalmmolecule - i.e. it is always a 4d slice of a continuant, never an event
% 
% taking as an example a 4D-slice of a hydron in :
% http://www.reactome.org/cgi-bin/eventbrowser?DB=gk_current&FOCUS_SPECIES=Homo%20sapiens&ID=372511&
% 
% H___clathrin_sculpted_acetylcholine_transport_vesicle_lumen_ --[entityReference]--> H___ChEBI_15378_ --[xref]--> biopax#ChEBI_15378 --[{db,id}]--> {ChEBI,15378}
%
% for pathways, there is no event reference;
%
%                                   Neurotransmitter_Release_Cycle --[xref]--> neurotransmitter_secretion --[{db,id}]--> {GO,...}
%
% this is because xref has the domain {CV or E or EntityReference or Evidence or Provenance}
%   (which seems a little weird!)
%
% we treat reactome 'entity's as 4D snapshots of continuants. We equate the reference with the continuant.
% both continuants and events are 'entity's in the metadata_db sense, and thus can have xrefs (via the xref property),
% but we follow the xref property to make the OBO-style ID
metadata_db:entity_label(E,L):- biopax_name(E, L).
metadata_db:entity_xref(E,XA):- xref(E,X),db(X,DB),id(X,ID),db_fix(DB,DB2),concat_atom([DB2,ID],':',XA).
metadata_db:entity_comment(E,X):- biopax_comment(E, X).
pathway_db:entity_db_id(Ref,DB,ID) :- xref(Ref,X),db(X,DB),id(X,ID).

% e.g. H___clathrin_sculpted_acetylcholine_transport_vesicle_lumen_ --> H___ChEBI_15378_
% we can think of this as a 4D entity to its type
%pathway_db:entity_reference(E,Ref) :- entityReference(E,Ref).
pathway_db:snapshot_continuant(E,Ref) :- entityReference(E,Ref).

pathway_db:event(X) :- pathway(X).
%pathway_db:event(X) :- biochemicalReaction(X).
pathway_db:event(X) :- interaction(X).

interaction_type(X,control) :- control(X).
interaction_type(X,catalyst) :- catalysis(X).


pathway_db:preceded_by(P2,P1):- PS1 stepProcess P1, PS1 nextStep PS2, PS2 stepProcess P2.

pathway_db:event_catalyst(E,C,CI,Type) :- interaction(CI),controlled(CI,E),controller(CI,C),interaction_type(CI,Type).

pathway_db:event_input(E,C,Num) :- left(E,C),participantStoichiometry(E,Stoi),physicalEntity(Stoi,C),stoichiometricCoefficient(Stoi,NumA),atom_number(NumA,Num).
pathway_db:event_input(E,C,1) :- left(E,C),\+ ((participantStoichiometry(E,Stoi),physicalEntity(Stoi,C))).

pathway_db:event_output(E,C,Num) :- right(E,C),participantStoichiometry(E,Stoi),physicalEntity(Stoi,C),stoichiometricCoefficient(Stoi,NumA),atom_number(NumA,Num).
pathway_db:event_output(E,C,1) :- right(E,C),\+ ((participantStoichiometry(E,Stoi),physicalEntity(Stoi,C))).

%TODO
%pathway_db:regulates(Regulator,Regulated) :- controlled(CI,Regulated),stepProcess

pathway_db:subpathway_of(P,W) :- pathwayComponent(W,P).

pathway_db:located_in(C,L) :- cellularLocation(C,L).

pathway_db:has_part(W,P,Num) :- component(W,P),componentStoichiometry(W,Stoi),physicalEntity(Stoi,P),stoichiometricCoefficient(Stoi,NumA),atom_number(NumA,Num).
pathway_db:has_part(W,P,1) :- component(W,P),\+ ((componentStoichiometry(W,Stoi),physicalEntity(Stoi,P))).
pathway_db:has_part(W,P,1) :- physicalEntity(W,P).

pathway_db:is_protein(C) :- biopax3_db:protein(C).
pathway_db:is_complex(C) :- biopax3_db:complex(C).
pathway_db:is_small_molecule(C) :- biopax3_db:smallMolecule(C).
pathway_db:is_dna(C) :- biopax3_db:dna(C).
pathway_db:is_dna_region(C) :- biopax3_db:dnaRegion(C).
pathway_db:is_rna(C) :- biopax3_db:rna(C).
pathway_db:is_rna_region(C) :- biopax3_db:rnaRegion(C).

pathway_db:molecule_modification(C,M) :- biopax3_db:sequenceFeature(C,M).
pathway_db:modification_type(M,TX) :- modificationType(M,T),entity_xref(T,TX). % e.g. CHEBI xref to phosphate
%pathway_db:modification_target(M,Loc) :- sequencePosition(M,Loc).
pathway_db:modification_target(M,Loc) :- featureLocation(M,Loc).
pathway_db:target_type(L,TX) :-
	regionType(L,T),entity_xref(T,TX). % e.g. CHEBI xref to tyrosine

