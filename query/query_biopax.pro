:- use_module(bio(biopax2_db)).
:- use_module(bio(biopax2_rules)).
:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).
:- use_module(bio(bioprolog_util)).

%entity_class_modelclass(E,C,MC):-
%        bp_entity_class(E,C),
%        rdf_has

idfrag(ID,Frag):-
        concat_atom([_,Frag],'#',ID).

bp2go(P,C):-
        bp2go(P,C,_).
bp2go(P,C,Xref):-
        P xref X,
        X db DB,
        X id ID,
        concat_atom([DB,ID],':',Xref),
        class_xref(C,Xref).

p2f(PC,PCN,FC,FCN,P,Org):-
        p2f(PC,FC,P,Org),
        class(PC,PCN),
        class(FC,FCN).

p2f_redundant(PC,FC,Org):-
        p2f(PC,FC,_,Org),
        parentT(P2C,PC),
        p2f(P2C,F2C,_,Org),
        parentRT(F2C,FC).

p2f(PC,FC,PShort,Org):-
        pathway(P),
	pathway_organism(P,Org),
        concat_atom([_,PShort],'#',P),
        bp_entity_class(P,PC),
        belongs(PC,biological_process),
        has_subpathwayT(P,F),
        bp_entity_class(F,FC),
        belongs(FC,molecular_function).

pathway_organism(P,Org):-
        P organism OrgID,
        concat_atom([_,Org],'#',OrgID).
pathway_organism(P,unknown):-
        \+ (P organism _).


p2f_orgs(PC,FC,POrgs):-
        solutions(p2f(PC,FC,P,Org),p2f(PC,FC,P,Org),OL),
        solutions(p2f(PC,FC),member(p2f(PC,FC,_,_),OL),L),
        member(p2f(PC,FC),L),
        solutions(P-Org,member(p2f(PC,FC,P,Org),OL),POrgs).

p2f_orgs(PC,FC,PCN,FCN,POrgs):-
        p2f_orgs(PC,FC,POrgs),
        class(PC,PCN),
        class(FC,FCN).

% always p2p..
p2fc(PC,FC,CC,PShort,Org):-
        pathway(P),
        P organism OrgID,
        concat_atom([_,Org],'#',OrgID),
        concat_atom([_,PShort],'#',P),
        P pathway_components Step,
        Step step_interactions F,
        (   F left PEP
        ;   F right PEP),
        ontxref(P,PC),
        ontxref(F,FC),
        PEP cellular_location Loc,
        ontxref(Loc,CC).

p2fcN(PC-PCN,FC-FCN,CC-CCN,PShort,Org):-
        p2fc(PC,FC,CC,PShort,Org),
        class(CC,CCN),
        class(PC,PCN),
        class(FC,FCN).

% TODO: reasoning step, incl adjacency
cc_span(C1,C2,S,F):-
        F left PEP1,
        F right PEP2,
        PEP1 cellular_location Loc1,
        PEP2 cellular_location Loc2,
        Loc1 \= Loc2,
        ontxref(Loc1,C1),
        ontxref(Loc2,C2),
        (   parentT(C1,C2)
        ->  S='<'
        ;   parentT(C2,C1)
        ->  S='>'
        ;   S='.').

cc_spanR(C1,C2,FC,S,FN):-
        F left PEP1,
        F right PEP2,
        PEP1 cellular_location Loc1,
        PEP2 cellular_location Loc2,
        Loc1 \= Loc2,
        ontxref(F,FC),
        ontxref(Loc1,C1),
        ontxref(Loc2,C2),
        idfrag(F,FN),
        (   parent(C1,R,C2)
        ->  S=R
        ;   parent(C2,R,C1)
        ->  S= -R
        ;   S='.').

cc_spanRc(C1,C2,EC1,EC2,FC,S,FN):-
        F left PEP1,
        F right PEP2,
        PEP1 cellular_location Loc1,
        PEP2 cellular_location Loc2,
        Loc1 \= Loc2,
        ontxref(F,FC),
        ontxref(Loc1,C1),
        ontxref(Loc2,C2),
        PEP1 physical_entity E1,
        E1 cpeRT E1t,
        ontxref(E1t,EC1),
        PEP2 physical_entity E2,
        E2 cpeRT E2t,
        ontxref(E2t,EC2),
        %xr(E1t,EC1),
        %xr(E2t,EC2),
        idfrag(F,FN),
        (   parent(C1,R,C2)
        ->  S=R
        ;   parent(C2,R,C1)
        ->  S= -R
        ;   S='.').

xr(E,C):- ( ontxref(E,C) -> true ; C=E).

cc_spanN(C1-C1N,C2-C2N,S,F):-
        cc_span(C1,C2,S,F),
        class(C1,C1N),
        class(C2,C2N).

pathway_consistency(Check,PClass,SubPClass,N1,N2,P,SubP):-
        pathway(P),
        bp_entity_class(P,PClass),
        has_subpathwayT(P,SubP),
        bp_entity_class(SubP,SubPClass),
        (   parent_overT(part_of,SubPClass,PClass)
        ->  Check=ok
        ;   Check=inconsistent),
        class(PClass,N1),
        class(SubPClass,N2).

pathway_xp_xref(P,PID,R,ChC,PC):-
        reaction_output(R,ChC),
        %debug(biopax,'~w ~w',[R,ChC]),
        /*
        subclassRT(ChC,ChCParent),
        differentium(PC,'OBO_REL:results_in_formation_of',ChCParent),
        % most specific
        \+ ( ( subclassRT(ChC,ChCX),
               differentium(PC,'OBO_REL:results_in_formation_of',ChCX),
               subclassT(ChCX,ChCParent))),*/
        differentium(PC,'OBO_REL:results_in_formation_of',ChC),
        debug(biopax,'   GO:~w ~w ~w',[R,PC,ChC]),
        %forall(has_subpathwayT(P1,R),
        %       \+ bp_entity_class(P1,_)),
        has_subpathway(P,R),
        \+  bp_entity_class(P,_),
        reactome_id(P,PID).
%        has_subpathwayT(P,R).

reactome_id(P,PID):-
        P xref X,
        X db 'Reactome',
        X id PID,
        \+ sub_atom(PID,0,_,_,'REACT').

        

pathway_new_xref(P,PID,PN,PC):-
        pathway(P),
        P bp_name PN,
        \+ bp_entity_class(P,_),
        bp_entity_class_lexmatch(P,PC),
        P xref X,
        X db 'Reactome',
        X id PID.

pathway_lacking_xref(P,PID,PN):-
        pathway(P),
        \+ bp_entity_class(P,_),
        P bp_name PN,
        P xref X,
        X db 'Reactome',
        X id PID.

pathway_consistency_lexmatch(Check,PClass,SubPClass,N1,N2,P,SubP):-
        pathway(P),
        bp_entity_class_lexmatch(P,PClass),
        has_subpathwayT(P,SubP),
        bp_entity_class(SubP,SubPClass),
        (   parent_overT(part_of,SubPClass,PClass)
        ->  Check=ok
        ;   Check=inconsistent),
        class(PClass,N1),
        class(SubPClass,N2).
        
xref_in_reactome_but_not_go(P,C):-
        bp_entity_class(P,C),
        \+ bp2go(P,_).

pathway_class_check(P,X,C,not_in_go):-
        ontxref(P,C),  % reactome-sourced mapping
        oboxref(P,X),
        atom_concat('Reactome:',_,X), % reactome ID
        \+ entity_xref(C,X). % no mapping in GO
pathway_class_check(P,X,C,consistent):-
        entity_xref(C,X),
        atom_concat('Reactome:',_,X),
        oboxref(P,X),
        ontxref(P,C).
pathway_class_check(P,X,C,not_in_reactome):-
        entity_xref(C,X), % mapping in GO
        atom_concat('Reactome:',_,X),
        oboxref(P,X), % get reactome internal rdf id
        \+ ontxref(P,C). % not in GO



bp_class_name_ont(P,C,N,Ont):-
        %bp2go(P,C),
        bp_entity_class(P,C),
        belongs(C,Ont),
        class(C,N).

        
