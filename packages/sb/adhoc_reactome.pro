
% ad-hoc queries for reactome

:- use_module(pathway_db).
:- use_module(interaction_db).
:- use_module(bio(metadata_db)).
:- use_module(bio(ontol_db)).
:- use_module(bio(curation_db)).
:- use_module(bio(bioprolog_util)).



protein_binds_to(P,C) :-
	interacts_withS(P,C1),
	subclass(C1,C).

protein_binding_class(P,GOID) :-
	protein_binds_to(P,C),
        class(G,binding),
        binding_relation(Rel),
        CDef=cdef(G,[Rel=C]),
        cdef_subsumed_by(CDef,GOID).
	
% pre-reasoned
protein_binding_class_nr(P,GOID) :-
	protein_binding_class(P,GOID),
	debug(pbc,'potential: ~w -> ~w',[P,GOID]),
	\+ ((curation_statement(_,P,_,X),
	     parent(X,GOID))),
	\+ ((protein_binding_class(P,X),
	     X\=GOID,
	     parent(X,GOID))).



% type:
% blip -r go -i Homo_sapiens.owl -u biopax3_bridge_to_pathway -u pathway_db findall PRED/ARITY

multi_xref(GO,E,E2,RID,RID2) :-
        event(E),entity_xref(E,GO),entity_xref(E2,GO),class(GO),event(E2),E2\=E,
        subpathway_ofT(E,E2),
        entity_db_id(E,'Reactome',RID),
        entity_db_id(E2,'Reactome',RID2),
        \+ sub_atom(RID,0,_,_,'REACT'),
        \+ sub_atom(RID2,0,_,_,'REACT').

inferred_xref(E,RID,BP) :-
        synthesis(_S,C,E),
        entity_xref(C,CX1),
        (   entity_alternate_identifier(CX,CX1)
        ;   CX=CX1),
        subclassRT(CX,CP),
        differentium(BP,'OBO_REL:has_output',CP),
        \+ ((subpathway_ofRT(E,EP),
             entity_xref(EP,BP2),
             parentRT(BP2,BP))),
        entity_db_id(E,'Reactome',RID).

%% protein_uniprot_id(P,U)
% e.g. P = 'http://www.reactome.org/biopax#hSMUG1_glycosylase__nucleoplasm_',
%      U = 'Q53HV7'
protein_uniprot_id(P,U) :-
        is_protein(P),
        snapshot_continuant(P,C),
        entity_db_id(C,'UniProt',U).

protein_uniprot_id_full(P,U) :-
        is_protein(P),
        snapshot_continuant(P,C),
	entity_xref(C,U),
	id_idspace(U,'UniProtKB').

% X is a GO class
% E is the Reactome event
% U is a UniProtKB protein ID
% R is a role
%
% based on Reactom subpathway_of hierarchy - so if U participates in an event E', and E' is a subpathway of E, then this predicate
% holds for E,U.
% Note that this may result in some 'true path' violations.
% E.g. {ERK1, ERK2} activation is a subpathway of {Axon guidance, Singaling by {EGFR, Insulin Receptor, PDGF, NGF} }.
% OK - 
go_event_protein_role(X,E,U,R) :-
        event_participating_continuant_roleT(E,C,R),
	entity_xref(C,U),
	id_idspace(U,'UniProtKB'),
	event_goxref(E,X),
	\+ sub_atom(U,_,_,_,'_'). % exclude e.g. UniProtKB:UCK1_HUMAN

go_event_protein_roles(X,E,U,Rs) :-
        setof(R,go_event_protein_role(X,E,U,R),Rs).

% blip-findall  -r go_assoc_local/goa_human_norm -r go  -r reactome/Homo_sapiens -u adhoc_reactome go_event_protein_roles_compare/5 -label
% blip-findall -table_pred ontol_db:parentT/2 -r go_assoc_local/goa_human_norm -r go_public  -r reactome/Homo_sapiens -u adhoc_reactome go_event_protein_roles_compare/5 -label
go_event_protein_roles_compare(X,E,UX,Rs,Srcs) :-
        go_event_protein_roles(X,E,U,Rs),
        atom_concat('UniProtKB:',U,UX),
        solutions(Src,
                  (   curation_statementT(Annot,UX,_,X),
                      curation_publisher(Annot,Src)),
                  Srcs).

snapshot_chebi(S,X) :-
        snapshot_continuant(S,C),
        entity_xref(C,X),
        id_idspace(X,'CHEBI').

results_in_transport_of(P,C) :-
	transport(P,_,_,_,_,C).

%transport_type

%r2go(phosphorylation,P,RID,GOID) :-

r2go(signaling,P,RID,GOID) :-
	pathway_starts_with(P,E),
        debug(r2go,'P ~w starts_with ~w',[P,E]),
        entity_xref(P, RID),
        is_pathway_id(RID),
	event_goxref(E,EX),
	PartDiffs=[],
	differentium(GOID,starts_with,_),
	genus(GOID,G), % TODO - this is cheating
        CDef=cdef(G,
                  ['OBO_REL:starts_with'=EX|
		  PartDiffs]),
        cdef_subsumed_by(CDef,GOID).

% TEST:
r2go(signaling2,P,RID,GOID) :-
	differentium(GOID,starts_with,EX2),
        debug(r2go,'P testing ~w',[EX2]),
	subclass(EX,EX2),
	event_goxref(E,EX),
        debug(r2go,'  starts? ~w',[E]),
	pathway_starts_with(P,E),
        debug(r2go,'  P ~w starts_with ~w',[P,E]),
        entity_xref(P, RID),
        is_pathway_id(RID).



r2go(signaling3,P,RID,GOID) :-
	pathway_starts_with(P,E),
        debug(r2go,'P ~w starts_with ~w',[P,E]),
        entity_xref(P, RID),
        is_pathway_id(RID),
	event_goxref(E,EX),
	subclass(EX,EX2),
	differentium(GOID,starts_with,EX2).




% blip-findall -debug xr2go -r reactome/Homo_sapiens -r implied/go_xp_all -u adhoc_reactome r2go/4 -label
r2go(transport,P,RID,GOID) :-
        transport(P,_S1,L1,_S2,L2,C),
        event_pathway_id(P,RID),
        entity_xref(L1, L1X),
        entity_xref(L2, L2X),
        entity_xref(C, CX),
        %class(G,transport),
        class(G,'vesicle-mediated transport'), % TEST
        CDef=cdef(G,
                  ['OBO_REL:results_in_transport_of'=CX,
                   'OBO_REL:results_in_transport_from'=L1X,
                   'OBO_REL:results_in_transport_to'=L2X]),
        cdef_subsumed_by(CDef,GOID).


r2go(biosynthesis,P,RID,GOID) :-
        synthesis(P,C,_),
        entity_xref(P,RID),
        is_pathway_id(RID),
        entity_xref(C, CX),
        class(G,'biosynthetic process'),
        synthesis_relation(Rel),
        CDef=cdef(G,[Rel=CX]),
        cdef_subsumed_by(CDef,GOID).

r2go(binding,P,RID,GOID) :-
        results_in_binding_of(P,C),
        entity_xref(P,RID),
        is_pathway_id(RID),
        class(G,binding),
        binding_relation(Rel),
        CDef=cdef(G,[Rel=C]),
        cdef_subsumed_by(CDef,GOID).


r2go(phosphorylation,P,RID,GOID) :-
        phosphporylation(P,C,_),
        entity_xref(P,RID),
        is_pathway_id(RID),
        entity_xref(C, CX),
        class(G,binding),
        binding_relation(Rel),
        CDef=cdef(G,[Rel=CX]),
        cdef_subsumed_by(CDef,GOID).

r2go(catalysis,Interaction,RID,GOID) :-
        event_catalyst(P,_,Interaction,catalyst),
        entity_xref(P, RID),
        is_pathway_id(RID),
        % this will only be a subset of the inputs/outputs (e.g. excluding proteins)
        setof(IX,I^(event_input(P,I),snapshot_chebi(I,IX)),IL),
        setof(OX,O^(event_output(P,O),snapshot_chebi(O,OX)),OL),
        class(G,'catalytic activity'),
        setof('OBO_REL:has_input'=X,member(X,IL),IL1),
        setof('OBO_REL:has_output'=X,member(X,OL),OL1),
        append(IL1,OL1,DL1),
        CDef1=cdef(G,DL1),
        % reverse
        setof('OBO_REL:has_output'=X,member(X,IL),IL2),
        setof('OBO_REL:has_input'=X,member(X,OL),OL2),
        append(IL2,OL2,DL2),
        CDef2=cdef(G,DL2),
        (   cdef_subsumed_by(CDef1,GOID,true)
        ;   cdef_subsumed_by(CDef2,GOID,true)
        ;   GOID=G),
	GOID \= 'GO:0003824'. % exclude trivial inferences


r2go_nr(Type,P,RID,GOID) :-
        r2go(Type,P,RID,GOID),
        debug(reactome,'~q. % now testing for nr',[r2go(Type,P,RID,GOID)]),
        \+ ((r2go(Type,P,RID,GOID_s),
             subclass(GOID_s,GOID),
             GOID_s\=GOID)).

% blip-findall -table_pred r2go/4 -debug xr2go -r reactome/Homo_sapiens -r implied/go_xp_all -u adhoc_reactome r2go_nr_xref_match/6 -label 
r2go_nr_xref_match(Type,P,RID,GOID,CurrentXref,Match) :-
        r2go_nr(Type,P,RID,GOID),
        (   event_goxref(P,GOID)
        ->  CurrentXref=GOID,
            Match=exact
        ;   event_goxref(P,CurrentXref)
        ->  (   subclass(CurrentXref,GOID)
            ->	Match=current(subclass) % inference is less specific
            ;   parent(CurrentXref,Rel,GOID)
            ->  Match=current(Rel)
            ;   subclass(GOID,CurrentXref)
            ->  Match=new(subclass)
            ;   parent(GOID,Rel,CurrentXref)
            ->  Match=new(Rel)
            ;   Match=inconsistent)
        ;   CurrentXref=none,
            Match=no_existing_xref).

event_goxref(P,GOID) :-
        entity_xref(P,GOID),
        id_idspace(GOID,'GO').
event_goxref(P,GOID) :-
        event_catalyst(P,_,C),
        entity_xref(C,GOID),
        id_idspace(GOID,'GO').
% humancyc biopax export does not have go xrefs
event_goxref(P,GOID) :-
	entity_xref(P,PX),
	id_idspace(PX,'HUMANCYC'),
	id_localid(PX,CYC_ID),
	% switch humancyc->metacyc
	concat_atom(['MetaCyc',CYC_ID],':',XrefInGO),
	entity_xref(GOID,XrefInGO).
event_goxref(Interaction,GOID) :-
	event_catalyst(P,_,Interaction,catalyst),
	entity_xref(P,PX),
	id_idspace(PX,'HUMANCYC'),
	id_localid(PX,CYC_ID),
	% switch humancyc->metacyc
	concat_atom(['MetaCyc',CYC_ID],':',XrefInGO),
	entity_xref(GOID,XrefInGO).

% TODO: unify
binding_relation('UCDHSC:results_in_joining_of').
binding_relation('results_in_joining_of'). % not expanded in go_xp_all
binding_relation('OBO_REL:results_in_binding_of').
synthesis_relation('OBO_REL:has_output').
synthesis_relation('OBO_REL:results_in_formation_of').

is_reactome_id(RID) :-
        id_idspace(RID,'Reactome'),
        \+ sub_atom(RID,0,_,_,'Reactome:REACT').
is_humancyc_id(RID) :-
        id_idspace(RID,'HUMANCYC').

is_pathway_id(RID) :- is_reactome_id(RID).
is_pathway_id(RID) :- is_humancyc_id(RID).
is_pathway_id(RID) :- id_idspace(RID,'CPATH').


event_pathway_id(P,RID) :-
	event_xref(P,RID),
	is_pathway_id(RID).
event_pathway_id(P,P).  % TODO - netpath


cdef_subsumed_by(C,P) :-
        cdef_subsumed_by(C,P,false).

%% cdef_subsumed_by(+CDef,?ClassID,+Complete:boolean)
% check if CDef is subsumed by ClassID - i.e. all the differentia in CDef are satisfied in the differentia of ClassID
% see match_all_differentia/3 for def of Complete
cdef_subsumed_by(C,P,Complete) :-
        C=cdef(GC,C_DL),
        debug(r2go,'checking if ~w < ~w',[C,P]),
        genus(P,GP),
        (   subclass(GC,GP)
        ;   cdef_subsumed_by(C,GP,Complete)),
        findall(R=ToP,differentium(P,R,ToP),P_DL),
        match_all_differentia(C_DL,P_DL,Complete).

%% match_all_differentia(?C_DL:list, +P_DL:list, Complete) is nondet
% make sure every differentium in P_DL is satisfied in C_DL.
% each differentium in C_DL can only be used once
% (consider 'nucleoside phosphate kinase activity' for the reason).
% if Complete is true then each differentium MUST be used
% may not be the most efficient implementation - may have to try all
% permutations for pairs of relationships.
match_all_differentia(_,[],false).
match_all_differentia([],[],true). % all must be accounted for
match_all_differentia(C_DL,[R=ToP|P_DL],Complete) :-
        select(R=ToC, C_DL, C_DL_2),
        subclass(ToC,ToP),
	debug(r2go,'  MATCHED[~w] ~w < ~w',[R,ToC,ToP]),
        match_all_differentia(C_DL_2,P_DL,Complete).
match_all_differentia(C_DL,[R=ToP|P_DL],Complete) :-
	transitive_over(R,R2),
	debug(r2go,'  ~w->~w',[R,R2]),
	property_equiv(R2,R3),
        select(R=ToC, C_DL, C_DL_2),
	restriction(ToC,R3,ToP), % pre-reasoned
	debug(r2go,'  MATCHED[~w] ~w < ~w (over ~w)',[R,ToC,ToP,R3]),
        match_all_differentia(C_DL_2,P_DL,Complete).

%property_equiv(X,R) :-
%	entity_xref(R,X).
property_equiv(X,X).

% TODO - fix temp hack next iteration
ontol_db:transitive_over('OBO_REL:results_in_transport_from',part_of).
ontol_db:transitive_over('OBO_REL:results_in_transport_to',part_of).


interaction_from_biogrid(X1,X2,Sys,Src,PMID,Tax) :-
        interaction(_,_,G1,G2,_,_,Sys,Src,PMID,Tax,Tax),
        entity_label(E1,G1),
        entity_xref(E1,X1),
        id_idspace(X1,'UniProt'),
        entity_label(E2,G2),
        entity_xref(E2,X2),
        id_idspace(X2,'UniProt').

hydron(H) :- entity_xref(H,'CHEBI:15378').

nomap(F,E,X,GOID) :-
	abolish(r2go_nr_xref_match/6), % hacky...
	consult(F),
	event(E),
	entity_xref(E,X),is_reactome_id(X),
	event_goxref(E,GOID),
	\+r2go_nr_xref_match(_,_,X,_,_,_).

process_overlaps(P1,P2,X) :-
	parentRT(XP1,P1),
	entity_xref(EP1,XP1),
	pathway_overlaps(EP1,EP2,X),
	entity_xref(EP2,XP2),
	parentRT(XP2,P2).

% TCA cycle and oxidative phosphorylation
% blip -u adhoc_reactome -r reactome/Homo_sapiens -r go findall "process_adjacent_to('GO:0006099','GO:0006119',X1,X2)" -label
process_adjacent_to(P1,P2,X1,X2) :-
	parentRT(XP1,P1),
	entity_xref(EP1,XP1),
	pathway_adjacent_to(EP1,EP2,X1,X2),
	entity_xref(EP2,XP2),
	parentRT(XP2,P2).

subpathway_of_go(X1,X2) :-
	subpathway_of(P1,P2),
	event_goxref(P1,X1),
	event_goxref(P2,X2).

subpathway_of_go_cons(X1,X2) :-
	subpathway_of(P1,P2),
	event_goxref(P1,X1),
	event_goxref(P2,X2),
	parent_overRT(part_of,X1,X2).

% X1 and Y1 are GO classes.
% the corresponding reactome events stand in a subpathway_of relationship.
% the GO classes are not related by subclass
subpathway_of_go_cons2(X1,X2) :-
	subpathway_of(P1,P2),
	event_goxref(P1,X1),
	event_goxref(P2,X2),
	\+ subclassRT(X1,X2).

pathway_pro(Path,ProClass,Role) :-
	event_participating_continuant_roleT(Path,Protein,Role),
	%debug(r2go,'Path-C ~w ~w',[Path,Protein]),
	entity_xref(Protein,UP),
	subclass(UP,ProClass),
	debug(r2go,'  Path-Pro ~w ~w',[Path,ProClass]),
	id_idspace(ProClass,'PRO').

pathway_pro_nr(Path,ProClass,Role) :-
	pathway_pro(Path,ProClass,Role),
	\+ ((pathway_pro(Path,ProClass2,Role),
	     ProClass2 \= ProClass,
	     subclass(ProClass2,ProClass))).
	

signaling_mismatch(Proc,Bind,RID,Path,Starts) :-
	differentium(Proc,starts_with,Bind),
	event_goxref(Path,Proc),
	%debug(r2go,'PP ~w ~w',[Path,Proc]),
	\+ ((subclass(Bind,Bind2),
	     event_goxref(E,Bind2),
	     subpathway_ofT(E,Path),
	     pathway_starts_with(Path,E))),
	solutions(E2,(pathway_starts_with(Path,E2),\+has_subevent(E2,_)),Starts),
        entity_xref(Path, RID),
        is_pathway_id(RID).


% todo: only include complex substances
synthesis(P,C,S_out) :-
	% must be the direct output of a process (i.e. not part of a composite)
        event_output(P,S_out),
        snapshot_continuant(S_out,C),
	% check that C was not present at the beginning (either standalone or part of a composite)
        \+ ((event_input_over_has_part(P,S_in),
             snapshot_continuant(S_in,C))),
	% finally, check that the synthesis was not from a more complex mol to a more simple one
	% (if we do not have a molecule count, assume this was complex enough...)
	% note for this we need chebi_with_formula...
	\+ ((chemical_atoms(C,AtomsOut),
	     debug(chem,'ca ~w = ~w',[C,AtomsOut]),
	     event_input(P,S_in_W),
	     has_partRT(S_in_W,S_in),
             snapshot_continuant(S_in,C_in),
	     % it's OK if the more complex thing is a different input
	     \+ ((event_output_over_has_part(P,S_in_2),
		  snapshot_continuant(S_in_2,C_in))),
	     chemical_atoms(C_in,AtomsIn),
	     debug(chem,'testing if ~w >= ~w',[AtomsIn,AtomsOut]),
	     less_complex_than(AtomsOut,AtomsIn))).

event_output_over_has_part(P,S) :-
	event_output(P,S1),
	has_partRT(S1,S).
event_input_over_has_part(P,S) :-
	event_input(P,S1),
	has_partRT(S1,S).


chemical_atoms(C,AtomCountPairs) :-
	entity_xref(C,CX),
	subclass(CX,F),
	id_idspace(F,'FORMULA'),
	debug(chem,'F=~w',[F]),
	class_cdef(F,cdef(_,DL)),
	setof(A=Num,
	      member(card(has_part,Num,Num)=A,DL),
	      AtomCountPairs).

less_complex_than(L1,L2) :-
	member(A=_,L2),
	\+ member(A=_,L1),
	!.
less_complex_than(L1,L2) :-
	member(A=N2,L2),
	member(A=N1,L1),
	N1 < N2,
	!.

go_pc(GC) :-
	setof(GC,(belongs(GC,cellular_component),
		  subclassT(GC,'GO:0043234')), % protein complex
	      GCs),
	member(GC,GCs).

guess_complex_xref(RC,GC,ILen,LenR,LenGO) :-
	go_pc(GC),
	solutions(UPGO,curation_statement(_,UPGO,_,GC),UPsGO),
	member(UPGO,UPsGO),
	entity_xref(C,UPGO),
	snapshot_continuant(S,C),
	part_ofT(S,RC),
	located_in(RC,_), % hack to make up for stoichiometry hack
	%debug(complex,'testing ~w ~w ~w',[GC,UPGO,RC]),
	has_leaf_parts(RC,Parts),
	solutions(UP,(member(Part,Parts),
		      protein_uniprot_id_full(Part,UP)),
		  UPsR),
	member(UP,UPsR),
	debug(complex,'  [~w <=> ~w] R-parts: ~w G-parts:~w',[GC,RC,UPsR,UPsGO]),
	length(UPsR,LenR),
	length(UPsGO,LenGO),
	%TLen is LenR+LenGO,
	findall(UP1,(member(UP1,UPsR),member(UP1,UPsGO)),UPsI),
	length(UPsI,ILen),
	ILen>1,
	P is ILen / LenR,
	P > 0.5,
	P2 is ILen / LenGO,
	P2 > 0.3.



cplx_suffix(' complex').
cplx_suffix(' protein complex').

go_reactome_textmatch(G,C) :-
	belongs(G,cellular_component),
	entity_label_or_synonym(G,GL),
	downcase_atom(GL,GL2),
	cplx_suffix(S),
	atom_concat(L,S,GL2),
	entity_label(C,L2),
	downcase_atom(L2,L),
	has_part(C,_).


mireot_class(ID) :-
	mireot_ref_class(Y),
	\+ id_idspace(Y,'GO'),
	subclassRT(Y,ID),class(ID).

mireot_ref_class(Y) :-
	differentium(X,_,Y),
	id_idspace(X,'GO').
mireot_ref_class(Y) :-
	genus(X,Y),
	id_idspace(X,'GO').
mireot_ref_class(Y) :-
	snapshot_continuant(_,C),
	entity_xref(C,Y).



/*
meq_complex_than(_,[]) :- !.
meq_complex_than([],_) :- !, fail.
meq_complex_than([A=N1|T1],L2) :-
	select(A=N2,L2,T2),
	!,
	N1 >= N2,
	meq_complex_than(T1,T2).
meq_complex_than([_|T1],L2) :-
	meq_complex_than(T1,L2).


*/

	
	