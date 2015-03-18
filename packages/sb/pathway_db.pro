:- module(pathway_db,
          [
           event/1,
           
           preceded_by/2,
           event_participant_role/3,
           event_participant_roleT/3,
           event_catalyst/2,
           event_catalyst/3,
           event_catalyst/4,
           event_input/2,
           event_input/3,
           event_output/2,
           event_output/3,

           event_input_molecule/2,
           event_output_molecule/2,

	   event_xref/2,
	   snapshot_xref/2,
	   
	   atomic_event/1,
	   
           event_chain/1,
           
           reaction_inputs_outputs/3,
           
           event_participating_continuant_roleT/3,
           
           subpathway_of/2,
           subpathway_ofT/2,
           subpathway_ofRT/2,
           has_subevent/2,
           has_subeventT/2,
           has_subeventRT/2,
	   pathway_starts_with/2,
	   pathway_initiated_by/2,
	   pathway_initiated_by/3,
           pathway_partial_overlaps/2,
           pathway_partial_overlaps/3,
           pathway_overlaps/2,
           pathway_overlaps/3,
	   pathway_adjacent_to/2,
	   pathway_adjacent_to/4,
           located_in/2,

           molecule_modification/2,
	   modification_type/2,
	   modification_target/2,
	   target_type/2,

           results_in_binding_of/2,
           
           is_protein/1,
           is_complex/1,
           is_small_molecule/1,
           is_dna/1,
           is_dna_region/1,
           is_rna/1,
           is_rna_region/1,
           
           has_part/2,
           has_part/3,
           has_partT/2,
           has_partRT/2,
           has_part_or_subtype/2,
           has_part_or_subtypeT/2,
           has_part_or_subtypeRT/2,
	   part_of/2,
	   part_ofT/2,
           has_leaf_part/2,
           has_leaf_parts/2,

           has_subtype/2,
           
           phosphorylation/3,
           dephosphorylation/3,
           autophosphorylation/1,

	   self_catalyzing/1,
	   self_catalyzing/2,
	   
           transport/4,
           transport/6,
           transport_chain/3,

           transformation/3,
           
           transport_source/2,
           transport_destination/2,
           transport_cargo/2,

           snapshot_continuant/2,
           snapshot_db_id/3,
           entity_db_id/3
           ]).

:- use_module(bio(dbmeta)).
:- use_module(bio(metadata_db)).
:- use_module(bio(bioprolog_util),[call_unique/1,solutions/3]).

%% snapshot_continuant(E,Ref)
% maps a state-specific snapshot of an entity (e.g. Hydron in the cytosol) to the endurant/continuant (e.g. Hydron)
:- extensional(snapshot_continuant/2).

% TODO: reactome sometimes uses alt_ids from CHEBI; e.g. CHEBI:1

snapshot_xref(S,X) :- snapshot_continuant(S,Ref),entity_xref(Ref,X).

event_xref(E,X) :- entity_xref(E,X).
event_xref(E,X) :- event_catalyst(E,_,C),entity_xref(C,X).


%% snapshot_db_id(E,DB,ID)
%
% composes snapshot_continuant/2 and entity_db_id/3
%
% * Example: =|snapshot_db_id(cdk2_nucleoplasm,'UniProt','P12345')|=
% * Example: =|snapshot_db_id(H_cytosol,'CHEBI','12345')|=
snapshot_db_id(E,DB,ID) :- snapshot_continuant(E,Ref),entity_db_id(Ref,DB,ID).

%% entity_db_id(Ref,DB,ID)
% @deprecated?
:- extensional(entity_db_id/3).

%% event(?E)
% true if E is an event
:- extensional(event/1).

%% atomic_event(?E)
% true if E has no sub-events
atomic_event(E) :-
        event(E),
        \+ subpathway_of(_,E).

%% preceded_by(P2,P1)
% true if P2 is causally preceded_by P1
:- extensional(preceded_by/2).

%% preceded_byT(P2,P1)
% transitive closure over preceded_by/2
preceded_byT(A,B) :- preceded_by(A,B).
preceded_byT(A,B) :- preceded_by(A,X),preceded_byT(X,B).

%% succeeded_by(P1,P2)
% inverse of preceded_by/2
succeeded_by(P1,P2) :- preceded_by(P2,P1).

%% succeeded_byT(P2,P1)
% closure over succeeded_by/2
succeeded_byT(A,B) :- succeeded_by(A,B).
succeeded_byT(A,B) :- succeeded_by(A,X),succeeded_byT(X,B).

%% preceded_by_via(?E2,?E1,?M)
% @deprecated - use partially_supplied_by/3
preceded_by_via(E2,E1,M) :-
        preceded_by(E2,E1),
        event_output(E1,M),
        event_input(E2,M).

pathway_starts_with(P,E) :-
	has_subeventT(P,E),
	\+ ((preceded_by(E,E2),
	     subpathway_ofT(E2,P))).

pathway_initiated_by(P,E) :-
	pathway_initiated_by(P,E,_).

%% pathway_initiated_by(?P,?E,?E1)
% E1 preceded_by E, E1 is in the P but E is not
pathway_initiated_by(P,E,E1) :-
	has_subeventT(P,E1),
	\+ has_subevent(E1,_),
	preceded_by(E1,E),
	\+ subpathway_ofT(E,P).



%% partially_supplied_by(?E2,?E1,?M)
% true if at least one input of E2 is an output of E1 and preceded_by/2 holds.
% definition from Racunas et al.
partially_supplied_by(E2,E1,M) :-
        preceded_by(E2,E1),
	has_input(E2,M),
	has_output(E1,M).

%% partially_supplied_by(?E2,?E1)
% true if at least one input of E2 is an output of E1 and preceded_by/2 holds.
% definition from Racunas et al.
partially_supplied_by(E2,E1) :-
	partially_supplied_by(E2,E1,_).


%% directly_supplied_by(?E2,?E1)
% true if all inputs of E2 are outputs of E1 and preceded_by/2 holds.
% definition from Racunas et al.
directly_supplied_by(E2,E1) :-
        preceded_by(E2,E1),
	forall(has_input(E2,M),
	       has_output(E1,M)).

%% indirectly_supplied_by(?E2,?EL)
% true of EL is the union of all events in the closure of preceded_by/2,
% and every input in E2 is an output of some member of EL.
% definition from Racunas et al.
indirectly_supplied_by(E2,EL) :-
	event(E2),
	setof(E^preceded_byT(E2,E),EL),
	forall(has_input(E2,M),
	       (   member(E1,EL),
		   has_output(E1,M))).

% ENABLED_BY

%% partially_enabled_by(?E2,?E1,?M)
% true if at least one catalyst of E2 is an output of E1 and preceded_by/2 holds.
% definition from Racunas et al.
partially_enabled_by(E2,E1,M) :-
        preceded_by(E2,E1),
	has_catalyst(E2,M),
	has_output(E1,M).

%% partially_enabled_by(?E2,?E1)
% true if at least one catalyst of E2 is an output of E1 and preceded_by/2 holds.
% definition from Racunas et al.
partially_enabled_by(E2,E1) :-
	partially_enabled_by(E2,E1,_).


%% directly_enabled_by(?E2,?E1)
% true if all catalysts of E2 are outputs of E1 and preceded_by/2 holds.
% definition from Racunas et al.
directly_enabled_by(E2,E1) :-
        preceded_by(E2,E1),
	forall(has_catalyst(E2,M),
	       has_output(E1,M)).

%% indirectly_enabled_by(?E2,?EL)
% true of EL is the union of all events in the closure of preceded_by/2,
% and every catalyst in E2 is an output of some member of EL.
% definition from Racunas et al.
indirectly_enabled_by(E2,EL) :-
	event(E2),
	setof(E^preceded_byT(E2,E),EL),
	forall(has_catalyst(E2,M),
	       (   member(E1,EL),
		   has_output(E1,M))).



event_chain([E1,E2]) :-
        preceded_by(E2,E1).
event_chain([E1,E2|EL]) :-
        preceded_by(E2,E1),
        event_chain([E2|EL]).

continuant_thread(_,[],[]).
continuant_thread(C,[E|ER],[T|TR]) :-
        transformation(C,E,T),
        continuant_thread(C,ER,TR).


	

%% event_catalyst(E,C,CI,Type)
% should be renamed event_controller/4?
:- extensional(event_catalyst/4).

%% event_catalyst(E,C,CI)
% as event_catalyst/4
event_catalyst(E,C,CI) :- event_catalyst(E,C,CI,_).

% --- ROLES ---

%% event_catalyst(E,C)
% as event_catalyst/4
event_catalyst(E,C) :- event_catalyst(E,C,_).

%% event_input(E,M,Num:integer)
% true if event E takes Num copies of M as input
:- extensional(event_input/3).

%% event_input(E,C)
% as event_input/2, without cardinality
event_input(E,C) :- event_input(E,C,_).

%% event_input_molecule(?E,?M)
% composition of event_input/2 and has_leaf_part/2.
%
% if E takes a complex as input, then this is true if M is a molecule snapshot and part_of that complex
event_input_molecule(E,M) :- event_input(E,C),has_leaf_part(C,M).

%% event_output(E,C,Num:integer)
% true if event E takes Num copies of M as output
:- extensional(event_output/3).

%% event_output(E,C)
% see event
event_output(E,C) :- event_output(E,C,_).

%% event_output_molecule(?E,?M)
% composition of event_output/2 and has_leaf_part/2
event_output_molecule(E,M) :- event_output(E,C),has_leaf_part(C,M).

%% event_participant_role(E,C,Role)
event_participant_role(E,C,input) :- event_input(E,C).
event_participant_role(E,C,output) :- event_output(E,C).
event_participant_role(E,C,catalyst) :- event_catalyst(E,C).


%% event_participant(E,C)
event_participant(E,C) :- event_input(E,C).
event_participant(E,C) :- event_output(E,C).
event_participant(E,C) :- event_catalyst(E,C).

%% event_participating_molecule(E,M)
% here M is a snapshot
event_participating_moleculeT(E,M) :- has_subeventRT(E,E2),event_participant(E2,C),has_leaf_part(C,M).

%% event_participating_continuantT(E,Cont)
event_participating_continuantT(E,Cont) :- has_subeventRT(E,E2),event_participant(E2,C),has_partRT(C,M),snapshot_continuant(M,Cont).

%% event_participating_continuant_roleT(?E,?Cont,?Role)
% Cont participates in E (or a super-event of E) with role Role
event_participating_continuant_roleT(E,Cont,Role) :- has_subeventRT(E,E2),event_participant_role(E2,M,Role),snapshot_continuant(M,Cont).
event_participating_continuant_roleT(E,Cont,complex(Role)) :- has_subeventRT(E,E2),event_participant_role(E2,C,Role),has_partT(C,M),snapshot_continuant(M,Cont).

event_participant_roleT(E,M,Role) :- has_subeventRT(E,E2),event_participant_role(E2,M,Role).
event_participant_roleT(E,M,complex(Role)) :- has_subeventRT(E,E2),event_participant_role(E2,C,Role),has_partT(C,M).





continuant_lifetime(C,TList) :-
        snapshot_continuant(M,C),
        part_ofRT(M,MP),
        event_participant(E,MP),
        subpathway_ofRT(E,EP),
        continuant_lifetime(C,EP,TList,[EP]).
continuant_lifetime(C,E,[E|TList],CheckedL) :-
        (   preceded_by(E2,E),
            \+ member(E2,CheckedL),
            call_unique(event_participating_molecule(E,M)),
            snapshot_continuant(M,C)
        *-> continuant_lifetime(C,E2,TList,[E2|CheckedL])
        ;   TList=[]).
%continuant_lifetime(_C,_E,[],_).


        
% FIXME
transformation_of(S1,S2,E1,E2,C,LostMods,GainedMods) :-
        event_participating_molecule(E1,S1),
        solutions(Mod,molecule_modification(S1,Mod),Mods1),
        preceded_by(E2,E1),
        event_participating_molecule(E2,S2),
        solutions(Mod,molecule_modification(S2,Mod),Mods2),
        snapshot_continuant(S1,C),
        snapshot_continuant(S2,C),
        subtract(Mods1,Mods2,LostMods),
        subtract(Mods2,Mods1,GainedMods).



%% subpathway_of(P,W)
% includes reactions to pathways
:- extensional(subpathway_of/2).

subpathway_ofT(A,B) :- subpathway_of(A,B).
subpathway_ofT(A,B) :- subpathway_of(A,X),subpathway_ofT(X,B).

subpathway_ofRT(A,B) :- subpathway_ofT(A,B).
subpathway_ofRT(A,A) :- event(A).

has_subevent(W,P) :- subpathway_of(P,W).

has_subeventT(A,B) :- has_subevent(A,B).
has_subeventT(A,B) :- has_subevent(A,X),has_subeventT(X,B).

has_subeventRT(A,B) :- has_subeventT(A,B).
has_subeventRT(A,A) :- event(A).

%% pathway_overlaps(A,B)
% pathways overlap if they have events in common.
% this includes as a degenerate case subpwathway_ofRT/2.
% note this predicate is reflexive
pathway_overlaps(A,B) :-
	pathway_overlaps(A,B,_).
pathway_overlaps(A,B,X) :-
        subpathway_ofRT(X,A),
        subpathway_ofRT(X,B).

%% pathway_partial_overlaps(A,B)
% as pathway_overlaps/2, with the additional constraint that neither
% pathway is part of the other
pathway_partial_overlaps(A,B) :-
	pathway_partial_overlaps(A,B,_).
pathway_partial_overlaps(A,B,X) :-
        pathway_overlaps(A,B,X),
        \+ subpathway_ofRT(A,B),
        \+ subpathway_ofRT(B,A).

pathway_adjacent_to(A,B) :-
	pathway_adjacent_to(A,B,_,_).
pathway_adjacent_to(A,B,X1,X2) :-
        subpathway_ofRT(X1,A),
	preceded_by(X2,X1),
        subpathway_ofRT(X2,B).


%% located_in(C,L)
% corresponds to biopax3_db:cellularLocation/2
% relation between a snapshot and a continuant. The container is typically a cellular component.
:- extensional(located_in/2).

%% molecule_modification(?Mol,?Mod)
% true if snapshot of molecule Mol is distinguished by modification Mod
:- extensional(molecule_modification/2).

%% modification_type(?Mod,?Type)
%
% Type will typically be a CHEBI class ID
%
% Some typical types (and their labels):
%   * CHEBI:50066-L-cystinyl group
%   * CHEBI:30003-L-selenocysteinate residue
%   * CHEBI:52472-alpha-N-acetylneuraminosyl-(2->3)-beta-D-galactosyl-(1->4)-N-acetyl-D-glucosamine
%   * CHEBI:15444- (1->4)-alpha-D-glucan
%   * CHEBI:29337-azanide
%   * CHEBI:23574-decanoyl group
%   * CHEBI:50039-deoxyhypusinyl group
%   * CHEBI:28087-glycogen
%   * CHEBI:30770-half-cystyl group
%   * CHEBI:25456-myristoyl group
%   * CHEBI:25650-octanoyl group
%   * CHEBI:47982-pantetheine 4'-phosphate group
%   * CHEBI:35780-phosphate ion
%
% Also: PubChem Compound:65396, GLYCAN:G12373
:- extensional(modification_type/2).

%% modificiation_target(?Mod,?Target)
:- extensional(modification_target/2).

%% target_type(?Target,?Type)
% 
% type will typically be a class ID from CHEBI.
% e.g. an amino acid
:- extensional(target_type/2).

modification_target_type(Mod,Tgt,Type) :-
	modification_target(Mod,Tgt),
	target_type(Tgt,Type).


:- extensional(is_protein/1).
:- extensional(is_complex/1).
:- extensional(is_small_molecule/1).
:- extensional(is_dna/1).
:- extensional(is_dna_region/1).
:- extensional(is_rna/1).
:- extensional(is_rna_region/1).

%% has_subtype(General,Specific)
:- extensional(has_subtype/2).

%% has_part(W,P,Num)
:- extensional(has_part/3).

%% has_part(W,P)
has_part(W,P) :- has_part(W,P,_).

%% has_partT(W,P)
% transitive closure of has_partT/2
has_partT(A,B) :- has_part(A,B).
has_partT(A,B) :- has_part(A,X),has_partT(X,B).

%% has_partRT(W,P)
% reflexive transitive closure of has_partT/2
has_partRT(A,B) :- has_partT(A,B).
has_partRT(A,A).

has_part_or_subtype(W,P) :- has_part(W,P).
has_part_or_subtype(W,P) :- has_subtype(W,P).

has_part_or_subtypeT(W,P) :- has_part_or_subtype(W,P).
has_part_or_subtypeT(W,P) :- has_part_or_subtype(W,P1),has_part_or_subtypeT(P1,P).

has_part_or_subtypeRT(W,P) :- has_part_or_subtypeT(W,P).
has_part_or_subtypeRT(W,W).



%% has_leaf_part(+W,?Parts:set) is det
% set of all leaf-nodes in the partonomy for W.
% this is locally reflexive for all leaf parts
has_leaf_part(W,P) :- has_partRT(W,P),\+ has_part(P,_).

has_leaf_parts(W,Ps) :- setof(P,has_leaf_part(W,P),Ps).

%% part_of(?P,?W)
% inverse of has_part/2
part_of(P,W) :- has_part(W,P).

%% part_ofT(?P,?W)
% transitive closure of part_of/2
part_ofT(A,B) :- part_of(A,B).
part_ofT(A,B) :- part_of(A,X),part_ofT(X,B).

%% part_ofRT(W,P)
% reflexive transitive closure of part_ofT/2
part_ofRT(A,B) :- part_ofT(A,B).
part_ofRT(A,A).

%% transport(?Event,?Slice1,?Loc1,?Slice2,?Loc2,?Continuant)
% true if Event is a transportation reaction with Continuant as cargo.
% @param Event typically a reaction
% @param Slice1 4D instance of Continuant at beginning of process
% @param Loc1 start location
% @param Slice2 4D instance of Continuant at end of process
% @param Loc2 end location
% @param Continuant cargo (typically a reference entity such as CHEBI or GO)
transport(P,S1,L1,S2,L2,C) :-
	% include parts of complexes
        event_input(P,S1C),has_partRT(S1C,S1),event_output(P,S2C),has_partRT(S2C,S2),snapshot_continuant(S1,C),snapshot_continuant(S2,C),located_in(S1,L1),located_in(S2,L2),L2\=L1.
%        event_input_molecule(P,S1),event_output_molecule(P,S2),snapshot_continuant(S1,C),snapshot_continuant(S2,C),located_in(S1,L1),located_in(S2,L2),L2\=L1.
% STRICT: does not include parts of complexes
%        event_input(P,S1),event_output(P,S2),snapshot_continuant(S1,C),snapshot_continuant(S2,C),located_in(S1,L1),located_in(S2,L2),L2\=L1.


%% transport(P,L1,L2,C)
% as transport/6 
transport(P,L1,L2,C) :-
        transport(P,_S1,L1,_S2,L2,C).

%% transport_destination(?Event,?Location)
% see transport/6
transport_destination(P,L) :- transport(P,_,L,_).

%% transport_source(?Event,?Location)
% see transport/6
transport_source(P,L) :- transport(P,L,_,_).

%% transport_cargo(?Event,?Continuant)
% see transport/6
transport_cargo(P,L) :- transport(P,_,_,L). % generic or specific? eg http://www.reactome.org/cgi-bin/eventbrowser?DB=gk_current&ID=532673

paired_transport(L1,S1,E1,L2,S2,E2,L3,S3,C) :-
        transport(E1,L1,S1,L2,S2,C),transport(E2,L2,S2,L3,S3,C).

%% transport_chain(?C,?PL:list,?LL:list)
% true if C is a continuant which is passed along sequentially along the locations in LL via the events in PL
%
% can take a long time to enumerate all solutions as there are many transport chains
%
% Example:
% ==
% freeze(C,C\='http://www.reactome.org/biopax#H___ChEBI_15378_'),
%   transport_chain(C,PL,L),length(L,Len),Len>4.
% ==
transport_chain(C,PL,LL) :-
        transport_chain(C,PL,[],LL).

transport_chain(C,[P],VisitedPL,[L1,L2]) :-
        transport(P,L1,L2,C),
        \+ member(P,VisitedPL).
transport_chain(C,[P|PR],VisitedPL,[L1,L2|LR]) :-
        transport(P,L1,L2,C),
        debug(pathway,'~w',[transport(P,L1,L2,C)]),
        \+ member(P,VisitedPL),
        transport_chain(C,PR,[P|VisitedPL],[L2|LR]).

transformation(C,P,transport(L1,L2)) :-
        transport(P,L1,L2,C).
transformation(C,P,modification(Mod)) :-
        event_input_molecule(P,S1),
        event_output_molecule(P,S2),
        snapshot_continuant(S1,C),
        snapshot_continuant(S2,C),
        molecule_modification(S1,Mod),
        \+ (molecule_modification(S2,Mod)).
transformation(C,P,composition(Cx1,Cx2)) :-
        snapshot_continuant(S1,C),
        has_leaf_part(Cx1,S1),
        event_input(P,Cx1),
        event_output(P,Cx2),
        Cx1\=Cx2,
        has_leaf_part(Cx2,S2),
        snapshot_continuant(S2,C).

results_in_binding_of(P,C,S1,S2) :-
        event_input(P,S1),
        event_output(P,S2),
        has_part(S2,S1),
	% TODO - see Reactome:157027 - heterodimers - protein complex?
        snapshot_continuant(S1,C).

results_in_binding_of(P,C) :-
        results_in_binding_of(P,C,_,_).


%% event_input_output_modification(?Event,?Slice1,?Slice2,?Mod)
% Mod = +(M) or -(M)
event_input_output_modification(P,S1,S2, +(Mod)) :-
        event_input_molecule(P,S1),
        event_output_molecule(P,S2),
        snapshot_continuant(S1,C),
        snapshot_continuant(S2,C),
        molecule_modification(S2,Mod),
        \+ (molecule_modification(S1,Mod)).
event_input_output_modification(P,S1,S2, -(Mod)) :-
        event_input_molecule(P,S1),
        event_output_molecule(P,S2),
        snapshot_continuant(S1,C),
        snapshot_continuant(S2,C),
        molecule_modification(S1,Mod),
        \+ (molecule_modification(S2,Mod)).


% TODO - see 'http://www.reactome.org/biopax#Dephosphorylation_of_inactive_c_src_by_PTPB1'
% tricky - reactome bp3 export does not include 
phosphorylation(P,C,Type) :-
        event_input_output_modification(P,S1,_S2, +(Mod)),
        modification_type(Mod,ModT),
	is_phosphate_group(ModT),
	modification_target_type(Mod,_,Type),
        snapshot_continuant(S1,C).
dephosphorylation(P,C,Type) :-
        event_input_output_modification(P,S1,_S2, -(Mod)),
        modification_type(Mod,ModT),
	is_phosphate_group(ModT),
	modification_target_type(Mod,_,Type),
        snapshot_continuant(S1,C).

autophosphorylation(P) :-
	autophosphorylation(P,_,_).
	
autophosphorylation(P,C,Type) :-
	phosphorylation(P,C,Type),
	event_catalyst(P,C).
autophosphorylation(P,C,Type) :-
	phosphorylation(P,C,Type),
	self_catalyzing(P).

self_catalyzing(P) :-
	self_catalyzing(P,_).

self_catalyzing(P,S) :-
	event_catalyst(P,S),
	event_input(P,SI),
	has_partRT(SI,S).

	

aggregation_of(P,C1,C2) :-
        event_input(P,C1),
        event_input(P,C2),
        event_output(P,CU),
        has_leaf_parts(C1,L1),
        has_leaf_parts(C2,L2),
        union(L1,L2,LU),
        has_leaf_parts(CU,LU).

homodimerization_of(P,C) :- aggregation_of(P,C,C).

reaction_inputs_outputs(P,IL,OL) :-
        setof(I,event_input(P,I),IL),
        setof(O,event_output(P,O),OL).


is_phosphate_group('http://www.reactome.org/biopax#phosphate_group'). % DEPRECATED - use CHEBI
is_phosphate_group('CHEBI:32958'). % phosphate group
is_phosphate_group('CHEBI:35780'). % phosphate ion - BUG IN REACTOME

:- multifile dbmeta:fact_chain_hook/2.
dbmeta:fact_chain_hook(event(X),
		       [event_catalyst(X,_,_,_),
			metadata_db:entity_xref(X,_),
			metadata_db:entity_label(X,_),
			event_input(X,_,_),
			event_output(X,_,_),
			subpathway_of(_,X)]).
dbmeta:fact_chain_hook(event_catalyst(_,P,_,_),
		       [snapshot_continuant(P,_),
			metadata_db:entity_xref(P,_),
			metadata_db:entity_label(P,_),
			located_in(P,_),
			has_part(P,_,_)]).
dbmeta:fact_chain_hook(event_input(_,P,_),
		       [snapshot_continuant(P,_),
			metadata_db:entity_xref(P,_),
			metadata_db:entity_label(P,_),
			located_in(P,_),
			has_part(P,_,_)]).
dbmeta:fact_chain_hook(event_output(_,P,_),
		       [snapshot_continuant(P,_),
			metadata_db:entity_xref(P,_),
			metadata_db:entity_label(P,_),
			located_in(P,_),
			has_part(P,_,_)]).
dbmeta:fact_chain_hook(snapshot_continuant(_,C),
		       [metadata_db:entity_xref(C,_),
			metadata_db:entity_label(C,_)]).
dbmeta:fact_chain_hook(subpathway_of(X,_),
		       [event(X)]).
%dbmeta:fact_chain_hook(has_part(X,_),
%		       [event(X)]).


/** <module> represents biological pathways

  ---+ Synopsis

==
:- use_module(bio(pathway_db)).

% 
demo:-
  nl.
  

==

---+ Details

See exploring_pathways.txt

---+ Additional Information



*/
