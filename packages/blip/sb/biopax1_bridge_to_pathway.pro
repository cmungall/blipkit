:- module(biopax1_bridge_to_pathway,[]).

:- use_module(bio(biopax1_db)).
:- use_module(bio(metadata_db)).
:- use_module(bio(pathway_db),[]).

:- [biopax_idspace_replace_rules].

% pathwaycommons files often contain duplicated blocks
:- multifile user:message_hook/3.
user:message_hook(rdf(redefined_id(_)),_,_) :- !.

metadata_db:entity_label(E,L):- biopax_name(E, L).
metadata_db:entity_xref(E,XA):-
        xref(E,X),db(X,DB),id(X,ID),db_fix(DB,ID,DB2,ID2),concat_atom([DB2,ID2],':',XA1),
        % this horrible hack to deal with Rhea and CHEBI IDs....
        (   concat_atom([DB2,DB2,RealID],':',XA1)
        ->  concat_atom([DB2,RealID],':',XA)
        ;   XA=XA1).
metadata_db:entity_comment(E,X):- biopax_comment(E, X).
pathway_db:entity_db_id(Ref,DB,ID) :- xref(Ref,X),db(X,DB),id(X,ID).

% e.g. H___clathrin_sculpted_acetylcholine_transport_vesicle_lumen_ --> H___ChEBI_15378_
% we can think of this as a 4D entity to its type
pathway_db:snapshot_continuant(E,Ref) :- physical_entity(E,Ref).

pathway_db:event(X) :- pathway(X).
pathway_db:event(X) :- interaction(X).
%pathway_db:event(X) :- physicalInteraction(X).
%pathway_db:event(X) :- biochemicalReaction(X).
%pathway_db:event(X) :- interaction(X).

interaction_type(X,control) :- control(X).
interaction_type(X,catalyst) :- catalysis(X).

stoi_num(X,X) :- !. % TODO
stoi_num(m,m) :- !.
stoi_num(n,n) :- !.
%stoi_num(A,N) :- debug(bp,'stoi: "~w"',[A]),atom_number(A,N).

pathway_db:preceded_by(P2,P1):- PS1 step_interactions P1, PS1 next_step PS2, PS2 step_interactions P2.

pathway_db:event_catalyst(E,C,CI,Type) :- interaction(CI),controlled(CI,E),controller(CI,C),interaction_type(CI,Type).

pathway_db:event_input(E,C,Num) :- left(E,C),stoichiometric_coefficient(C,NumA),stoi_num(NumA,Num).
pathway_db:event_input(E,C,1) :- left(E,C),\+stoichiometric_coefficient(C,_).

pathway_db:event_output(E,C,Num) :- right(E,C),stoichiometric_coefficient(C,NumA),stoi_num(NumA,Num).
pathway_db:event_output(E,C,1) :- right(E,C),\+stoichiometric_coefficient(C,_).

pathway_db:subpathway_of(P,W) :- pathway_components(W,P).

pathway_db:located_in(C,L) :- cellular_location(C,L).

%pathway_db:has_part(W,P,Num) :- component(W,P),componentStoichiometry(W,Stoi),physical_entity(Stoi,P),stoichiometric_coefficient(Stoi,NumA),atom_number(NumA,Num).
%pathway_db:has_part(W,P,1) :- component(W,P),\+ ((componentStoichiometry(W,Stoi),physical_entity(Stoi,P))).
%pathway_db:has_part(W,P,1) :- physical_entity(W,P).

pathway_db:is_complex(C) :- biopax1_db:complex(C).
pathway_db:is_small_molecule(C) :- biopax1_db:smallMolecule(C).

%pathway_db:molecule_modification(C,M) :- biopax3_db:sequence_feature(C,M).
%pathway_db:modification_type(M,T) :- modificationType(M,T). % e.g. CHEBI xref
%pathway_db:modification_location(M,Loc) :- sequencePosition(M,Loc).


