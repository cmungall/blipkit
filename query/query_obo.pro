/* -*- Mode: Prolog -*- */
/**
  @author Chris Mungall
  @version @cvskw $Revision$
  @date @cvskw $Date$
  @license @link(url='http://www.fsf.org/licensing/licenses/lgpl.html')|LGPL|

  @s1|Name| query_obo - 

  @s1 Synopsis

  @cl
  :- use_module(query_obo).

  @/cl

  @s1 Description

  @cl
  blip -r go -u query_obo findall relationship_diamond/4
  @/cl
  
**/


:- use_module(bio(bioprolog_util)).
:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).
:- use_module(bio(curation_db)).
:- use_module(bio(seqfeature_db)).
:- use_module(bio(ontol_reasoner)).
:- use_module(bio(mode)).
:- use_module(bio(dbmeta)).
:- use_module(bio(graph)).
:- use_module(library(porter_stem),[]).


is_a_diamond(P,C1,C2):-
        subclass(P,C1),
        subclass(P,C2),
        C1 @< C2.

relationship_diamond(R,P-PN,C1-N1,C2-N2):-
        class(P,PN),
        property(R,_),
        parent_overT(R,P,C1),
        parent_overT(R,P,C2),
        C1 @< C2,
        \+ parent_overT(_,C1,C2),
        \+ parent_overT(_,C2,C1),
        class(C1,N1),
        class(C2,N2).

nonredundant_subclass(C,P):-
        subclass(C,P),
        \+ ((   subclassT(C,X),
                subclassT(X,P))).

univocal_violation(C1,C2,N):-
        class(C1,N),
        class(C2,N),
        C1\=C2.

% duplciated!! with changes
ontol_closure_predicate(all,type_parent(_)).
ontol_closure_predicate(R,ontol_db:parent_over_nr(R,_)):- property(R,_).
type_parent(T,ID,PID):- parent(ID,T,PID).

class_depth_profile(ID,N,R,MaxDepth,NumPaths):-
        class(ID,N),
        ontol_closure_predicate(R,RPred),
        solutions(Path,closure_path(RPred,Path,ID,_),Paths),
        length(Paths,NumPaths),
        solutions(Length,(member(Path,Paths),length(Path,Length)),Lengths),
        list_max(Lengths,MaxDepth).
        
dangling(ID):-
        parent(_,_,ID),
        \+class(ID,_).

class_name_def(C,N,D):-
        class(C,N),
        def(C,D).


% C not_in_organism T, T' isa T => C not_in_organism T'
% T lacks C, T' isa T => T lacks C
in_organism_inconsistency(C-N,CSuper-NSuper):-
        restriction(C,in_organism,T),
        parentRT(C,CSuper),
        restriction(CSuper,not_in_organism,TSuper),
        parentRT(T,TSuper),
        class(C-N),
        class(CSuper-NSuper).

% test for regaining (defeasible reasoning)
p(X,Y):- parent_overRT(part_of,X,Y).
p(X,Y):- subclassRT(X,Y).
lacks_inconsistency(in(C-N,T-TN),lacks(CSuper-NSuper,TSuper-TNSuper)):-
        restriction(C,in_organism,T),  % potential regain of C in T
        p(T,TSuper),     % the broad taxon in which the loss is defined
        restriction(TSuper,lacks,CSuper), % broad taxon lacks C or a more general type
        writeln(C-CSuper),
        p(C,CSuper),
        class(C,N),
        class(CSuper,NSuper),
        class(T,TN),
        class(TSuper,TNSuper).

% is_a diamond that violates disjointness
disjointness_violation(X-XN,C1-CN1,C2-CN2,P1-PN1,P2-PN2):-
        subclass(X,C1),
        subclass(X,C2),
        C1 @< C2,  % make sure inverses are stated
        subclassRT(C1,P1),
        disjoint_from(P1,P2),
        subclassRT(C2,P2),
        class(X,XN),
        class(C1,CN1),
        class(C2,CN2),
        class(P1,PN1),
        class(P2,PN2).

disjointness_violationNR(X-XN,C1-CN1,C2-CN2,P1-PN1,P2-PN2):-
        disjointness_violation(X-XN,C1-CN1,C2-CN2,P1-PN1,P2-PN2),
        \+ (( subclassT(Y,X),
              disjointness_violation(Y-_,_,_,_,_))).

disjointness_violationNR(X-XN,P1-PN1,P2-PN2):-
        disjointness_violationNR(X-XN,_,_,P1-PN1,P2-PN2).

disjoint_from_inductively_implied(C1,C1N,C2,C2N):-
        subclass(C1,P),
        subclass(C2,P),
        C1 \= C2,
        %\+ \+ subclass(_,C1),   % not leaf
        %\+ \+ subclass(_,C2),
        \+ ((   subclassT(A,C1),
                subclassT(A,C2))),
        class(C1,C1N),
        class(C2,C2N).

non_disjoint_siblings(C1,C1N,C2,C2N):-
        subclass(C1,P),
        subclass(C2,P),
        C1 \= C2,
        subclassT(A,C1),
        subclassT(A,C2),
        %class(A,AN),
        class(C1,C1N),
        class(C2,C2N).

non_disjoint_siblings_cc_root(C1,C1N,C2,C2N):-
        class(Root,cellular_component),
        subclass(C1,Root),
        non_disjoint_siblings(C1,C1N,C2,C2N).
        
non_disjoint_siblings_cc_subroot(C1,C1N,C2,C2N):-
        class(Root,cellular_component),
        subclass(C0,Root),
        subclass(C1,C0),
        non_disjoint_siblings(C1,C1N,C2,C2N).
        
disjoint_from_inductively_implied(C1,C2):-
        disjoint_from_inductively_implied(C1,_,C2,_).

disjoint_from_inductively_implied_cc_root(C1,C1N,C2,C2N):-
        class(Root,cellular_component),
        subclass(C1,Root),
        disjoint_from_inductively_implied(C1,C1N,C2,C2N).

disjoint_from_inductively_implied_cc_subroot(C1,C1N,C2,C2N):-
        class(Root,cellular_component),
        subclass(C0,Root),
        subclass(C1,C0),
        disjoint_from_inductively_implied(C1,C1N,C2,C2N).

all_subclasses_disjoint(C,CN,Num):-
        class(C),
        setof(SC,subclass(SC,C),SCs),
        length(SCs,Num),
        Num > 1,
        forall(subclass(SC1,C),
               forall(subclass(SC2,C),
                      is_disjoint_or_identical(SC1,SC2))),
        class(C,CN).

% todo
%split_topnodes_into_disjoint_sets(C):-
%        class(C),
%        setof(SC,subclass(SC,C),SCs),
%        findall(C1-C2,(subclass(C1,C),subclass(C2,C),C1\=C2,\+ non_disjoint(C1,C2)),DisjointPairs),
%        partition_pairs(DisjointPairs,DisjointSets).

partition_pairs([],[]):- !.
%partition_pairs(Pairs,Sets):-
        
        

is_disjoint_or_identical(X,X):- !.
is_disjoint_or_identical(X,Y):-
        \+ non_disjoint(X,Y).

non_disjoint(X,Y):-
        subclassT(A,X),
        subclassT(A,Y).

        


only_child_of(C-CN,P-PN):-
        subclass(C,P),
        \+ (   subclass(C2,P),
               C\=C2),
        class(C,CN),
        class(P,PN).

unpropagated_subset(ID-N,PID-PN,Subset):-
        entity_partition(ID,Subset),
        parent(ID,PID),
        \+ entity_partition(PID,Subset),
        class(ID,N),
        class(PID,PN).

explain_subclass(X,Y,Why):-
        explanation(subclass(X,Y),Why).


:- multifile user:eq/7.
align_match(S,X,Y,XN,YN):-   user:eq(S,X,Y,_,_,XN,YN).
align_match(S,X,Y,XN,YN):-   user:eq(S,Y,X,_,_,YN,XN).
align_match(xref,X,Y,XN,YN):- class_xref(X,Y),class(X,XN),class(Y,YN).
align_match(xref,X,Y,XN,YN):- class_xref(Y,X),class(X,XN),class(Y,YN).

% blip -r go -r fma -f tbl -i gocc_fma-align.txt findall -u query_obo inconsistency_by_alignment/6
inconsistency_by_alignment(X-XN,'is_a+',XP-XPN,Y-YN,'not-is_a+',YP-YPN):-
        align_match(S,X,Y,XN,YN),
        debug(query,'align_match ~w',[X-XN]),
        subclassT(X,XP),
        align_match(S,XP,YP,XPN,YPN),
        debug(query,'checking ~w ~w',[X-XP,Y-YP]),
        \+ subclassT(Y,YP).

%disjoint_subset(ID-N):-
%        class(ID,N),
%        subclassT(ID,RID),

class_xref_or_align(X,Y,xref):- class_xref(X,Y).
class_xref_or_align(X,Y,xref):- class_xref(Y,X).
class_xref_or_align(X,Y,align):- user:eq(_,X,Y,_,_,_,_).
class_xref_or_align(X,Y,align):- user:eq(_,Y,X,_,_,_,_).


ontology_mapping(Method,ID1,N1,Def1,ID2,N2,Def2):-
        class(ID1,N1) leftjoin def(ID1,Def1),
        class_xref_or_align(ID1,ID2,Method),
        class(ID2,N2) leftjoin def(ID2,Def2).

show_ontology_mapping:-
        solutions([ID1,N1,Def1]-[ID2,N2,Def2],ontology_mapping(_,ID1,N1,Def1,ID2,N2,Def2),L),
        maplist(show_ontology_mapping,L).
show_ontology_mapping(L1-L2):-
        append(L1,L2,L),
        T=..[ontology_mapping,Method|L],
        solutions(Method,T,Methods),
        writeln(Methods),
        writecols(L1),nl,
        writecols(L2),nl,
        nl.

double_replaced_by(E-N,Rep1-Rep1N,Rep2-Rep2N,Reason):-
        entity_replaced_by(E,Rep1),
        entity_replaced_by(E,Rep2),
        Rep1\=Rep2,
        entity_label(E,N),
        entity_label(Rep1,Rep1N),
        entity_label(Rep2,Rep2N),
        class_comment(E,Reason).

replace_chain(E1-N1,E2-N2,E3-N3):-
        entity_consider_or_replaced_by(E1,E2),
        entity_consider_or_replaced_by(E2,E3),
        entity_label(E1,N1),
        entity_label(E2,N2),
        entity_label(E3,N3).

single_is_a_path(X,[X]):-        \+ subclass(X,_).
single_is_a_path(X,[X|P]):-
        subclass(X,Y),
        \+ (subclass(X,Z),Z\=Y),
        single_is_a_path(Y,P).

single_is_a_parent(ID-N,PID-PN):-
        subclass(ID,PID),
        \+ ((subclass(ID,XID),
             XID\=PID)),
        class(ID,N),
        class(PID,PN).

single_is_a_child(ID-N,PID-PN):-
        subclass(ID,PID),
        \+ ((subclass(XID,PID),
             XID\=ID)),
        class(ID,N),
        class(PID,PN).

multiple_parentage_asserted(Ont,R,X-XN,Y1-Y1N,Y2-Y2N):-
        belongs(X,Ont),
        restriction(X,R,Y1),
        restriction(X,R,Y2),
        Y1 @< Y2,
        class(X,XN),
        class(Y1,Y1N),
        class(Y2,Y2N).

multiple_parentage_implied(Ont,R,X-XN,Y1-Y1N,Y2-Y2N):-
        belongs(X,Ont),
        parent_overT(R,X,Y1),
        R\=subclass,
        parent_overT(R,X,Y2),
        Y1 @< Y2,
        X\=Y1,
        X\=Y2,
        \+ parentT(Y1,Y2),
        \+ parentT(Y2,Y1),
        % most specific only
        \+ ((parentT(X,Z),
             parentT(Z,Y1),
             parentT(Z,Y2))),
        \+ ((parentT(X,Z1),
             parentT(X,Z2),
             (   Z2=Y2,parentT(Z1,Y1)
             ;   Z1=Y1,parentT(Z2,Y2)))),
        class(X,XN),
        class(Y1,Y1N),
        class(Y2,Y2N).

interesting(X-XN):-
        single_is_a_path(X,P),
        length(P,PLen),
        PLen>4,
        member(Y,P),
        restriction(Y,_,Z),
        single_is_a_path(Z,_),
        class(X,XN).

present_in_multiple_ontologies(N,C1-Res1,C2-Res2):-
        class_label_exact(C1,N),
        class_label_exact(C2,N),
        C1\=C2,
        belongs(C1,Res1),
        belongs(C2,Res2),
        Res1\=Res2.             % extra insurance: avoid "paralogs"
present_in_multiple_ontologies(N):-
        present_in_multiple_ontologies(N,_,_).

name_clash(N,C1,C2):-
        class_label_exact(C1,N),
        class_label_exact(C2,N),
        C1@<C2.


g_or_d(ID,X):- genus(ID,X).
g_or_d(ID,X):- differentium(ID,_,X).
  
xp_depth_max(IDMax,DMax):-
        findmax(ID,D,xp_depth(ID,D),IDMax,DMax).

xp_depth(ID,D1):-
        class(ID),
        findall(D,
                (   g_or_d(ID,X),xp_depth(X,D)),
                Ds),
        (   list_max(Ds,DMax)
        ->  D1 is DMax+1
        ;   D1=0 ).

total_subclasses(ID,Num):-
        class(ID),
        setof_count(X,subclassT(X,ID),Num).

total_subclasses_by_subroot(ID,N,Num):-
        noparent(R),
        subclass(ID,R),
        class(ID,N),
        total_subclasses(ID,Num).

total_subclasses_by_subsubroot(ID,N,Num):-
        noparent(Root),
        subclass(SubRoot,Root),
        subclass(ID,SubRoot),
        class(ID,N),
        total_subclasses(ID,Num).

suggested_xp(ID1-N1,Rels):-
        class(ID1,N1),
        \+ genus(ID1,_),
        findall(rel(R,To,ToN),
                (   parent(ID1,R,To),
                    class(To,ToN)),
                Rels),
        Rels=[_,_|_],
        \+ ((
             class(ID2),
             ID2\=ID1,
             forall(member(rel(R,To,_),Rels),
                    parentRT(ID2,R,To)))),
        format('[Term]\n'),
        format('id: ~w ! ~w\n',[ID1,N1]),
        forall(member(rel(R,To,ToN),Rels),
               (   (   R=subclass
                   ->  RN=''
                   ;   RN=R),
                   format('intersection_of: ~w ~w ! ~w~n',[RN,To,ToN]))),
        nl.




% basic - no inference
indistinguishable_by_dag(ID1-N1,ID2-N2,Rels):-
        class(ID1,N1),
        findall(rel(R,To,ToN),
                (   parent(ID1,R,To),
                    class(To,ToN)),
                Rels),
        Rels=[_,_|_],
        class(ID2,N2),
        ID2\=ID1,
        forall(subclass(ID1,X),
               subclassRT(ID2,X)),
        forall(restriction(ID1,R,To),
               parent_overT(R,ID2,To)).

% with parts
indistinguishable_by_dag2(ID1-N1,ID2-N2,Rels):-
        indistinguishable_by_dag(ID1-N1,ID2-N2,Rels1),
        findall(rel(rev(R),To,ToN),
                (   parent(To,R,ID1),
                    class(To,ToN)),
                Rels2),
        append(Rels1,Rels2,Rels),
        forall(restriction(From,R,ID1),
               parent_overT(R,From,ID2)).


% with parts, direct, no reasoning
% (only this one is correct - does reverse)
indistinguishable_by_dag3(ID1-N1,ID2-N2,Rels):-
        class(ID1,N1),
        findall(rel(R,To,ToN),
                (   parent(ID1,R,To),
                    class(To,ToN)),
                Rels),
        Rels=[_,_|_],
        class(ID2,N2),
        ID2\=ID1,
        forall(subclass(ID1,X),
               subclass(ID2,X)),
        forall(subclass(ID2,X),
               subclass(ID1,X)),
        forall(restriction(ID1,R,To),
               restriction(ID2,R,To)),
        forall(restriction(ID2,R,To),
               restriction(ID1,R,To)),
        forall(restriction(To,R,ID1),
               restriction(To,R,ID2)),
        forall(restriction(To,R,ID2),
               restriction(To,R,ID1)).

% determines if two XP defs are indiscernable
% (could also be done using ontol-reasoner)
indistinguishable_by_xp_def(ID1-N1,ID2-N2):-
        class(ID1,N1),
        genus(ID1,G1),
        genus(ID2,G1),
        ID2\=ID1,
        class(ID2,N2),
        forall(differentium(ID1,R,To),
               differentium(ID2,R,To)),
        forall(differentium(ID2,R,To),
               differentium(ID1,R,To)).

link_to_multiple_members_of_disjoint_set(X1-X1N,R,Z1-Z1N,X2-X2N,R,Z2-Z2N,Y1-Y1N,Y2-Y2N):-
        restriction(X1,R,Y1),
        restriction(X2,R,Y2),
        subclassRT(Y1,Z1),
        subclassRT(Y2,Z2),
        disjoint_from(Z1,Z2),
        class(X1,X1N),
        class(X2,X2N),
        class(Y1,Y1N),
        class(Y2,Y2N),
        class(Z1,Z1N),
        class(Z2,Z2N).

link_to_multiple_members_of_disjoint_set(X1-X1N,R,Z1-Z1N,X2-X2N,R,Z2-Z2N):-
        restriction(X1,R,Y1),
        restriction(X2,R,Y2),
        subclassRT(Y1,Z1),
        subclassRT(Y2,Z2),
        disjoint_from(Z1,Z2),
        class(X1,X1N),
        class(X2,X2N),
        class(Z1,Z1N),
        class(Z2,Z2N).
        

link_from_multiple_members_of_disjoint_set(X1-X1N,R,Z1-Z1N,X2-X2N,R,Z2-Z2N):-
        restriction(Y1,R,X1),
        restriction(Y2,R,X2),
        subclassRT(Y1,Z1),
        subclassRT(Y2,Z2),
        disjoint_from(Z1,Z2),
        class(X1,X1N),
        class(X2,X2N),
        class(Z1,Z1N),
        class(Z2,Z2N).

class_idspace(C,X):-
        atomic(C),
        concat_atom([X,_],':',C).

xref_stat(S,N) :-
        count_by(S,C-X,(class_xref(C,X),class_idspace(X,S)),L),
        member(S-N,L).
%        aggregate(count,(class_xref(C,X),class_idspace(X,S)),Num).


xp_stat(S):-
        xp_stats(L),member(S,L).

xp_stats(L):-
        count_by(idspace(S),X,(differentium(_,_,X),class_idspace(X,S)),L).
xp_stats(L):-
        count_by(uses(S),A-R-X,(differentium(A,R,X),belongs(X,S)),L).
xp_stats(L):-
        count_by(genus(S),X,(genus(X,_),class_idspace(X,S)),L).
xp_stats(L):-
        count_by(genus(S),X,(genus(X,_),belongs(X,S)),L).
xp_stats(L):-
        count_by(gd(S,T),X,(genus(X,_),differentium(X,_,Y),class_idspace(X,S),class_idspace(Y,T)),L).
xp_stats(L):-
        count_by(gd(S,T),X,(genus(X,_),differentium(X,_,Y),belongs(X,S),belongs(Y,T)),L).
xp_stats(L):-
        count_by(rel(R),X,Y^(differentium(X,R,Y)),L).

used_rel(R):-
        solutions(R,differentium(_,R,_),Rs),
        member(R,Rs).

rel_summary(Rel,NumUses,RelDef):-
        used_rel(Rel),
        setof_count(X,differentium(_,Rel,X),NumUses),
        (   property(Rel)
        ->  (   def(Rel,RelDef)
            ->  true
            ;   RelDef='NO_DEF')
        ;   RelDef='NO_REL').

rel_summary(Rel,NumUses,NumClasses,RelDef,GenusClasses):-
        rel_summary(Rel,NumUses,RelDef),
        solutions(G,(differentium(X,Rel,_),genus(X,G)),GenusClasses),
        setof_count(X,differentium(X,Rel,_),NumClasses).

link_stat(S):-
        count_by(rel(R),A-B,(restriction(A,R,B)),L),
        member(S,L).



agg(avg).
agg(min).
agg(max).
agg(with(max)).

rel_stat(R,Agg,parents,Val):-
	property(R),
	agg(Agg),
	aggregate_by(Agg,X,parent(X,R,_Y),Val).
rel_stat(R,children,Agg,Val):-
	property(R),
	agg(Agg),
	aggregate_by(Agg,Y,parent(_X,R,Y),Val).

parentRelX(X,R,Y):- parent(X,R1,Y),class_xref(R,R1).
parentRelX(X,R,Y):- parent(X,R1,Y),class_xref(R1,R).
parentRelX(X,R,Y):- parent(X,R,Y).

structural_diff(AC,AR,AP,BC,BR,BP):-
        structural_diff1(AC,AR,AP,BC,BR,BP).
bstructural_diff(AC,AR,AP,BC,BR,BP):-
        structural_diff2(AC,AR,AP,BC,BR,BP).

% A: base ontology - this has the xrefs
% B: 2nd ontology - this must conform to A
structural_diff1(Ac-AcN,R,Ap-ApN,Bc-BcN,R2,Bp-BpN):-
        class_xref(Ac,Bc),
        parentRelX(Ac,R,Ap),
        class_xref(Ap,Bp),
        \+ (parentRelX(Bc,R,Bp)), % exact same structure?
        belongs(Bc,Bo),
        belongs(Bp,Bo),
        class(Bc,BcN),
        class(Bp,BpN),
        class(Ac,AcN),
        class(Ap,ApN),
        (   parentRT(Bc,R2,Bp)  % path exists?
        ->  true                % path exists but not identical
        ;   R2= not/R).         % path does not exist

% switched around - A must conform to B
structural_diff2(Ac-AcN,R2,Ap-ApN,Bc-BcN,R,Bp-BpN):-
        class_xref(Ac,Bc),
        parentRelX(Bc,R,Bp),
        class_xref(Ap,Bp),
        \+ (parentRelX(Ac,R,Ap)),
        belongs(Ac,Ao),
        belongs(Ap,Ao),
        class(Bc,BcN),
        class(Bp,BpN),
        class(Ac,AcN),
        class(Ap,ApN),
        (   parentRelRT(Ac,R2,Ap)
        ->  true
        ;   R2= not/R).

% the following over a reasoned db:

nr_sdiff_infdb(A,R,B,AX,RX,BX) :-
        sdiff_infdb(A,R,B,AX,RX,BX),
        \+ ((sdiff_infdb(A,_,Z,_,_,_),
             parent(Z,B),
             Z\=B)).

sdiff_infdb(A,R,B,AX,RX,BX) :-
        class_xref(A,AX),
        \+ \+ parent(AX,_),     % ext is loaded
        parent(A,R,B),
        A\=B,                   % no reflexive
        class_xref(B,BX),
        \+ \+ parent(BX,_),     % ext is loaded
        \+ parent(AX,R,BX),
        (   parent(AX,RX,BX)
        ->  true
        ;   RX=not(R)).
sdiff_infdb(A,R,B,AX,RX,BX) :-
        class_xref(AX,A),
        parent(A,R,B),
        A\=B,                   % no reflexive
        class_xref(BX,B),
        \+ parent(AX,R,BX),
        (   parent(AX,RX,BX)
        ->  true
        ;   RX=not(R)).

        

identical_text_def(X-XN,Y-YN,Def):-
        def(X,Def),
        def(Y,Def),
        X @< Y,
        class(X,XN),
        class(Y,YN).

near_identical_text_def(X-XN,Y-YN,Diffs,Def,XDef,YDef):-
        findall(X-Def,(def(X,Def1),porter_stem:atom_to_stem_list(Def1,Toks),concat_atom(Toks,Def)),XDefs),
        findall(X-Y-Def,(member(X-Def,XDefs),member(Y-Def,XDefs),X @< Y),XYDefs),
        member(X-Y-Def,XYDefs),
        class(X,XN),
        class(Y,YN),
        def(X,XDef),
        def(Y,YDef),
        porter_stem:tokenize_atom(XDef,XToks),
        porter_stem:tokenize_atom(YDef,YToks),
        findall(XT,(member(XT,XToks),\+member(XT,YToks)),XTs),
        findall(YT,(member(YT,YToks),\+member(YT,XToks)),YTs),
        append(XTs,YTs,Diffs).

parent_by_xref(Ont,C,R,P):-
        belongs(C,Ont),
        entity_xref(C,CX),
        parent(CX,R,PX),
        entity_xref(P,PX),
        belongs(P,Ont),
        \+ parentRT(C,P),
        debug(query,'~w ~w ~w',[C,R,P]).

parent_by_xref_nr(Ont,C,R,P):-
        solutions(C-R-P,parent_by_xref(Ont,C,R,P),CRPs),
        member(C-R-P,CRPs),
        \+ ( (member(C-_-P2,CRPs),
              parentT(P2,P))).

subclass_by_xref_confirmed(Ont,C,P):-
        belongs(C,Ont),
        entity_xref(C,CX),
        entity_xref(C,CY),
        belongs(CX,OntX),
        belongs(CY,OntY),
        OntX\=OntY,
        subclass(CX,PX),
        entity_xref(P,PX),
        \+ subclassRT(C,P),
        entity_xref(P,PY),
        subclassT(CY,PY).

cycle_with_names(ID,P,XNs):-
        parent_cycle(ID,P),
        findall(N,(member(X-_-_,P),class(X,N)),XNs).

bad_obsolete_metadata(ID,X):-
        entity_obsolete(ID,_),
        entity_consider_or_replaced_by(ID,X),
        entity_obsolete(X,_).

missing_is_a_withdef(ID,N,Def):-
        class(ID,N),
        \+ subclass(ID,_),
        (   def(ID,Def)
        ->  true
        ;   Def='').

two_part_of_parents(X,Y,Z):-
        two_parents(part_of,X,Y,Z).

two_parents(R,X,Y,Z):-
        restriction(X,R,Y),
        restriction(X,R,Z),
        Y\=Z,
        \+ subclass(Y,Z),
        \+ subclass(Z,Y).


nonfunctional(R,X,Y,Z):-
        restriction(X,R,Y),
        restriction(X,R,Z),
        Y\=Z.

% blip -i uberon_edit.obo -u query_obo findall 'addsyn(C,Diff)' -select "Diff" -write_prolog
% blip-ddb -i uberon_edit.obo -goalfile $< io-convert -to obo
addsyn(C,Diff) :-
        class(C),
        solutions(SynD,
                  (   entity_synonym_scope(C,Syn,exact),
                      downcase_atom(Syn,SynD),
                      \+ class(C,SynD),
                      SynD\=Syn,
                      \+ entity_synonym_scope(C,SynD,exact)),
                  NewSyns),
        member(NewSyn,NewSyns),
        (   Diff=assert(entity_synonym_scope(C,NewSyn,exact))
        ;   Diff=assert(entity_synonym(C,NewSyn))).

        

% blip -i uberon_edit.obo -r fma -u query_obo findall 'xrefmove(C,P,P2)' -select "(retract(subclass(C,P)),assert(subclass(C,P2)))" -write_prolog
xrefmove(C,P,C2) :-
        subclass(C,P),
        entity_xref(P,PX),
        entity_xref(C,CX),
        subclassT(CX,C2X), % or asserted?
        entity_xref(C2,C2X),
        subclassT(C2X,PX),
        subclassT(C2,P).

xrefmove(R,C,P,C2) :-
        restriction(C,R,P),
        entity_xref(P,PX),
        entity_xref(C,CX),
        parent_overT(R,CX,C2X), % or asserted?
        entity_xref(C2,C2X),
        parent_overT(R,C2X,PX),
        parent_overT(R,C2,P).

xrefadd(R,C,P) :-
        entity_xref(C,CX),
        restriction(CX,R,PX),
        entity_xref(P,PX),
        \+ restriction(C,R,_),
        \+ parentRT(C,P),
        \+ ((subclassT(C,C2),
            restriction(C2,R,_))).

xrefadd(R,C,P) :-
        entity_xref(C,CX),
        restriction(CX,R,PX),
        entity_xref(P,PX),
        \+ restriction(C,R,_),
        \+ parentRT(C,P),
        \+ ((subclassT(C,C2),
            restriction(C2,R,_))).

xrefaddR(R,C,P) :-
        class(C),
        entity_xref(C,CX),
        restriction(CX,R,PX),
        entity_xref(P,PX),
        \+ restriction(C,R,_),
        \+ parent(C,P).

xp_align(A,subclass,B,XA,not(subclass),XB) :-
        sole_differentium(A,R,XA),
        genus(A,G),
        subclassT(A,B),
        sole_differentium(B,R,XB),
        genus(B,G),
        \+ subclassT(XA,XB).

xp_align(A,not(subclass),B,XA,subclass,XB) :-
        sole_differentium(A,R,XA),
        genus(A,G),
        subclassT(XA,XB),
        sole_differentium(B,R,XB),
        genus(B,G),
        \+ subclassT(A,B).

xp_align_nr(A,subclass,B,XA,RX,XB) :-
        xp_align(A,subclass,B,XA,RX,XB),
        \+ (( xp_align(A2,subclass,B2,_,_,_),
              \+ ((A2=A,B2=B)),
              subclassRT(A,A2),
              subclassRT(B2,B))).

xp_align_nr(A,R,B,XA,subclass,XB) :-
        xp_align(A,R,B,XA,subclass,XB),
        \+ (( xp_align(_,_,_,XA2,subclass,XB2),
              \+ ((XA2=XA,XB2=XB)),
              subclassRT(XA,XA2),
              subclassRT(XB2,XB))).

sole_differentium(A,R,XA) :-
        differentium(A,R,XA),
        setof(R2-XA2,differentium(A,R2,XA2),[_]).

% For Uberon, CL etc:
% relies on reasoning

xref_align(A,subclass,B,XA,not(subclass),XB) :-
        entity_xref(A,XA),
        subclass(A,B),
        entity_xref(B,XB),
        \+ subclass(XA,XB).

xref_align_nr(A,subclass,B,XA,RX,XB) :-
        xref_align(A,subclass,B,XA,RX,XB),
        \+ (( xref_align(A2,subclass,B2,_,_,_),
              \+ ((A2=A,B2=B)),
              subclass(A,A2),
              subclass(B2,B))).

% assume xref ontology is JEPD
xref_disjoint_violation(X,A,B) :-
        subclassRT(X,A),
        entity_xref(A,XA),
        subclass(XA,XP),
        subclass(XB,XP),
        XA\=XB,
        entity_xref(B,XB),
        subclassRT(X,B).

xref_disjoint_violation_nr(X,A,B) :-
        xref_disjoint_violation(X,A,B),
        \+ ((xref_disjoint_violation(X2,A2,B2),
             subclassT(X,X2),
             subclassRT(A,A2),
             subclassRT(B,B2))).

% assume xref ontology is JEPD
jepd_xref_disjoint_violation(X,A,B) :-
        subclassRT(X,A),
        entity_xref(A,XA),
        subclass(XA,XP),
        subclass(XB,XP),
        XA\=XB,
        entity_xref(B,XB),
        subclassRT(X,B).

jepd_xref_disjoint_violation_nr(X,A,B) :-
        jepd_xref_disjoint_violation(X,A,B),
        \+ ((jepd_xref_disjoint_violation(X2,A2,B2),
             subclassT(X,X2),
             subclassRT(A,A2),
             subclassRT(B,B2))).

% slow
xref_disjoint_from_violation(XA,XB,V) :-
        disjoint_from(A,B),
        entity_xref(A,XA),
        class(XA),
        entity_xref(B,XB),
        class(XB),
        debug(disjoint,'testing ~w ~w',[XA,XB]),
        subclassRT(V,XA),
        subclassRT(V,XB).

xref_disjoint_from_violation_nr(X,A,B) :-
        xref_disjoint_from_violation(X,A,B),
        \+ ((xref_disjoint_from_violation(X2,A2,B2),
             subclassT(X,X2),
             subclassRT(A,A2),
             subclassRT(B,B2))).

xref_disjoint_over_violation(DR,XA,XB,V) :-
        disjoint_over(DR,R),
        (   restriction(A,DR,B)
        ;   restriction(B,DR,A)),
        debug(disjoint,'testing ~w ~w ~w',[A,DR,B]),
        subclassRT(A2,A),
        entity_xref(A2,XA),
        class(XA),
        subclassRT(B2,B),
        entity_xref(B2,XB),
        class(XB),
        debug(disjoint,'   --> ~w ~w ~w',[R,XA,XB]),
        parent_overRT(R,V,XA),
        parent_overRT(R,V,XB).

feature_annots_under(F,C,Num,MaxIC) :-
        aggregate(count,F1,A^R^C1^curation_statement(A,F1,R,C1),TotalFs),
        feature(F),
        solutions(C1,
                  (   curation_statement(_,F,_,C1),
                      parentRT(C1,C)),
                  C1s),
        solutions(CP,(member(C1,C1s),parentRT(C1,CP)),CPs),
        length(CPs,Num),
        Num>0,
        findall(Prob,(member(C1,C1s),
                      aggregate(count,F1,A^R^curation_statement(A,F1,R,C1),NumFs),
                      Prob is NumFs/TotalFs),
                Probs),
        min_list(Probs,MinP),
        MaxIC is -(log(MinP)/log(2)).

unique_annots_to_pair(CX,CY,GX,GY) :-
        curation_statementT(_,GX,_,CX),
        \+ curation_statementT(_,GX,_,CY),
        curation_statementT(_,GY,_,CY),
        \+ curation_statementT(_,GY,_,CX).
        
class_refcount(C,Num) :-
	(   aggregate(count,X,parent(X,C),Num)
	->  true
	;   Num=0).

% v-hacky - does not take into account diff.rel
redundant_isa_by_gd(A,B) :-
	subclass(A,B),
	genus(A,GA),
	genus(B,GB),
	subclassRT(GA,GB),
	forall(differentium(A,_,XA),
	       (   differentium(B,_,XB),
		   subclassRT(XA,XB))).

reldiff(A,B,Diff) :-
	reldiff1(A,B,Diff).
reldiff(A,B,Diff) :-
	reldiff1(B,A,Diff).

reldiff1(A,B,unique(A,X)) :-
	parent(A,X),
	\+ parent(B,X).
reldiff1(A,B,unique(X,A)) :-
	parent(X,A),
	\+ parent(X,B).


ontol_stat(S,num_classes,Num) :- aggregate(count,X,(class(X),id_idspace(X,S)),Num).
ontol_stat(S,num_xp_defined,Num) :- aggregate(count,X,G^(class(X),genus(X,G),id_idspace(X,S)),Num).
ontol_stat(S,pct_xp_defined,Pct) :-
	ontol_stat(S,num_xp_defined,Num),
	ontol_stat(S,num_classes,Tot),
	Pct is floor((Num/Tot)*100 + 0.5).

