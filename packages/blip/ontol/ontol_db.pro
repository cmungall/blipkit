/* -*- Mode: Prolog -*- */
:- module(ontol_db,
          [
           ontology/2,
           ontology/3,
           import_directive/1,
           import_all_ontologies/0,
           class/1,
           subclass/2,
           equivalent_class/2,
           restriction/3,
           restriction/4,
           restriction/5,
           cardinality_restriction/4,
           min_cardinality_restriction/4,
           max_cardinality_restriction/4,
           range_cardinality_restriction/4,
           belongs/2,
           synonym/3,
           def_xref/2,
           class_xref/2,
           disjoint_from/2,
           disjoint_over/2,
           class_disjoint_union_list/2,
           disjoint_from_violation/3,
           disjoint_over_violation/4,
           relation/1,
           property/1,
           property_domain/2,
           property_range/2,
           complement_of/2,
           holds_temporally_between/3,
           holds_atemporally_between/3,
           lexical_category/2,
           is_unsatisfiable/1,
           is_anonymous/1,
           inverse_of/2,
           inverse_of_on_instance_level/2,
           class_level_inverse_of/2,
           holds_bidirectionally_for/2,
           holds_bidirectionally_for/3,
           holds_over_chain/2,
           equivalent_to_chain/2,
           transitive_over/2,
           metaproperty/2,
           is_metadata_tag/1,
           is_class_level/1,
           is_transitive/1,
           is_symmetric/1,
           is_asymmetric/1,
           is_reflexive/1,
           is_anti_symmetric/1,
           is_functional/1,
           is_inverse_functional/1,
           is_cyclic/1,
           is_nondangling/1,
           is_proper/1,
           transitive_form_of/2,
           proper_form_of/2,
           cyclic_form_of/2,
           cyclic_over/2,
           reflexive_over/2,
           directed_simple_path_over/2,
           directed_path_over/2,
           id_axiom/2,
           all_some/1,
           holds_for_all_times/1,
	   property_relationship/3,
           obsolete/3,
           obsolete_class/2,
           idspace/2,
           inst_of/2,
           inst_of_at/3,
           inst_rel/3,
           inst_rel/4,
           inst_rel/5,
           inst_rel/6,
           inst_rel_at/4,
           inst_rel_anon/3,
           inst_rel_type/3,
           inst_sv/3,
           inst_sv/4,
           inst/1,
           class_union_element/2,
           class_intersection_element/2,
           property_intersection_element/2,
           property_intersection_elements/2,
           property_union_element/2,
           property_union_elements/2,
           genus/2,
           differentium/3,
           class_comment/2,
           def/2,
           class_instrule/3,
           class_reified_rulebody/3,
           reification/2,
           entailed_by/2,
           entailed_by/3,
           logicalformula/3,

	   merge_class/2,
	   merge_class/3,
	   
           remove_dangling_facts/0,
	   
           % intensional predicates
           class/2,
           property/2,
           inst/2,
           id_name/2,
           referenced_id/2,
           class_by_name_or_synonym/2,
           class_label/3,
           class_label_exact/2,
           topclass/1,
           topclass/2,
           noparent/1,
           noparent_and_haschild/1,
           noparent/2,
           noparent_and_haschild/2,
           noparent_by_type/2,
           nochild/1,
           nochild/2,
           subclassT/2,
           subclassRT/2,
           subclassX/2,
           subclassXT/2,
	   cdef_placement/5,
           subclassN/2,
           subclassTN/2,
           subclassRTN/2,
           child/2,
           child/3,
           node_link/3,
           restrictionN/3,
           parent_over/3,
           parent_over/4,
           parent_overT/3,
           parent_overRT/3,
           parent_over_nr/3,
           parent_over_nr/4,
           parent/2,
           parent/3,
           parentT/2,
           parentT/3,
           parentT/4,
           parentRT/2,
           parentRT/3,
           parentRT/4,
	   safe_parentT/2,
	   safe_parentT/3,
	   safe_parentT/4,
	   safe_parentRT/2,
	   safe_parentRT/3,
	   bf_parentRT/2,
	   strict_subclass/2,
	   class_single_inheritance_path/2,
	   cdef_label/2,
           class_cdef/2,
           
           subclass_cycle/2,
           parent_cycle/2,

           abduced_link/1,
           abduced_link_nr/1,
           
           dangling_class/1,
           redundant_subclass/3,
           redundant_parent/5,
           subclass_underlap/3,
           multiple_parent/4,
           multiple_parent/5,
           name_composed_type/2,

           ontology_segment/3,
           ontology_segment/4,

           subclass_lca/2,
           class_pair_subclass_lca/3,
           
           % experimental

           inst_ofRT/2,
           classdef_parent/2,
           classdef_subsumes_id/2,
           cdef/2
          ]).

% metadata on the data predicates used in this module
:- use_module(bio(dbmeta)).
:- use_module(bio(graph)).
:- use_module(bio(metadata_db)).
:- use_module(bio(bioprolog_util)).  % required for statistics
:- use_module(bio(macros_transitive)).

:- use_module(library(ordsets)).

:- pure
           class/2,
           property/2,
           slot/2,
           inst/2,
           id_name/2,
           referenced_id/2,
           class_by_name_or_synonym/2,
           class_label/3,
           class_label_exact/2,
           topclass/1,
           topclass/2,
           noparent/1,
           noparent/2,
           noparent_by_type/2,
           nochild/1,
           nochild/2,
           subclassT/2,
           subclassRT/2,
           subclassN/2,
           subclassTN/2,
           subclassRTN/2,
           child/2,
           child/3,
           node_link/3,
           restrictionN/3,
           parent_over/3,
           parent_over/4,
           parent_overT/3,
           parent_overRT/3,
           parent_over_nr/3,
           parent_over_nr/4,
           parent/2,
           parent/3,
           parentT/2,
           parentT/3,
           parentT/4,
           parentRT/2,
           parentRT/3,
           parentRT/4,

           cdef_label/2,
           class_cdef/2,
           
           subclass_cycle/2,
           parent_cycle/2,

           abduced_link/1,
           abduced_link_nr/1,
           
           dangling_class/1,
           redundant_subclass/3,
           redundant_parent/5,
           subclass_underlap/3,
           multiple_parent/4,
           multiple_parent/5,

           subclass_lca/2,
           class_pair_subclass_lca/3,
           
           % experimental

           inst_ofRT/2,
           classdef_parent/2,
           classdef_subsumes_id/2,
           cdef/2.



%  in progress: rewrite to use metadata_db
%
%  the convention is now to use
%
%  * predicate(ID) - ID is an instance of predicate [extensional]
%  * predicate(ID,Name) - intensional
%
%  some predicates are declared multifile, to support legacy factfiles
%  

%**************************************
%*              ONTOLOGIES               *
%***************************************

%%  ontology(?Ontology,?Name,?Desc) is nondet.
:- extensional(ontology/3).

%% ontology(?Ontology,?Name)
% see ontology/3  
ontology(ID,N):-
        ontology(ID,N,_).
ontology(ID,ID):-
        not(var(ID)),
        not(ontology(ID,_,_)).

:- extensional(import_directive/1).
:- dynamic ontol_db:import_directive/1.
:- dynamic already_imported/1.
import_all_ontologies:-
        findall(X,import_directive(X),Xs),
        import_ontologies(Xs,[]).
        
import_ontology(X):-
        debug(ontol,'importing "~w"',[X]),
        ensure_loaded(bio(io)),
        % massive hack. TODO!! fix. rdf_load does not appear to accept this
        (   sub_atom(X,_,_,_,'.obo')
        ->  Fmt=obo
        ;   Fmt=owl),
        (   sub_atom(X,0,_,_,'http:')
        ->  io:load_biofile(Fmt,url(X))
        ;   (   sub_atom(X,0,Pos,_,'file:')
            ->  sub_atom(X,Pos,_,0,File),
                io:load_biofile(Fmt,File)
            ;   io:load_biofile(Fmt,X))),
        assert(already_imported(X)).

import_ontologies([],_).
import_ontologies([File|Files],Parsed):-
        (   member(File,Parsed)
        ->  debug(ontol,'Already parsed ~w',[File])
        ;   import_ontology(File)),
        findall(X,(import_directive(X),\+member(X,Parsed)),Xs),
        debug(ontol,'~w imports: ~w',[File,Xs]),
        append(Xs,Files,Next),
        import_ontologies(Next,[File|Parsed]).

%**************************************
%*              CLASSES                  *
%***************************************

%%  class(?Class) is nondet.
% class declaration - true if Class is a class
:- extensional(class/1).

%%  class(?Class,?Label) is nondet.
%  combines class/1 and entity_label/2
:- multifile class/2.
class(C,Name):- entity_label(C,Name),class(C).

%%  subclass(?Class,?SuperClass) is nondet.
%    An asserted is_a (subtype, subsumption) relationship
%    equivalent to owl:subClassOf
%    transitive form is subclassT/2
:- extensional(subclass/2).
%:- transitive subclass/2.  % TODO!!! check : does not work with tabling??
%%  subclassT(?Class,?SuperClass) is nondet.
% transitive form of subclass/2
subclassT(X,Y):- subclass(X,Y).
subclassT(X,Y):- subclass(X,Z),subclassT(Z,Y).
subclassRT(X,X).
subclassRT(X,Y):- subclassT(X,Y).


%% subclassXT(?A,?B)
% reflexive version of subclassX/2
subclassXT(A,A).
subclassXT(A,B) :-
	debug(subclassX,'testing for subclassXT ~w < ~w',[A,B]),
	subclassX(A,B),
	debug(subclassX,'OK***** for subclassXT ~w < ~w',[A,B]).
	

%% subclassX(+A,+B)
%
% tests if A can be inferred to be a subclass of B
%
% if A and/or B are classes, will be translated into class expressions via class_cdef/2
% (alternatively, class expressions can be used directly)
%
% this can be used to test quickly where a new class expression should be placed in
% the ontology - given the ontology has all genus-differentia links already manifest
%
% e.g.
% *  subclassX( cdef(vw,[col=red]), cdef(car,[color=primary]) )
% *  subclassX( cdef(vw,[col=red]), bright_car ) where class_cdef(bright_car, ...)
% *  subclassX( red_vw, cdef(car,[color=primary]) )
% *  subclassX( red_vw, bright_car ) where class_cdef(bright_car, ...)
%
% to test 
subclassX(A,B) :-
	(   class_cdef(A,A1)
	;   A1=A),
	(   class_cdef(B,B1)
	;   B1=B),
	subclassX_2(A1,B1).
subclassX(A,B,asserted) :-
	subclassT(A,B).

% N+S conditions, any < intersection
subclassX_2(A,cdef(BG,BDs)) :-
	subclassXT(A,BG),
	forall(member(BD,BDs),
	       subclassXT(A,BD)).
% N+S conditions, class < rel-expr
subclassX_2(A,BR=BV) :-
	class(A),
	parent(A,AR,AV),
	subclassX_2(AR=AV,BR=BV).
        %% parent_overT(R,A,V). -- TODO
% N+S conditions, rel-expr < rel-expr
subclassX_2(AR=AV,BR=BV) :-
	subclassRT(AR,BR),
	subclassRT(AV,BV).
subclassX_2(AR=AV,BR=BV) :-
	is_transitive(BR),
	subclassRT(AR,BR),
	subclassRT(AV,X),
	parent(X,R,Y),
	subclassX_2(R=Y,BR=BV).
% N+S conditions, intersection < rel-expr [genus test]
subclassX_2(cdef(AG,_),R=V) :-
	subclassX_2(AG,R=V).
% N+S conditions, intersection < rel-expr [differentiae test]
subclassX_2(cdef(_,ADs),BR=BV) :-
	member(AR=AV,ADs),
	subclassX_2(AR=AV,BR=BV).
% N+S conditions, intersection < class
subclassX_2(cdef(AG,_),B) :-
	class(B),
	subclassRT(AG,B).

	

%% class_cdef(?C,?CDef) is nondet.
% true if CDef is asserted to be equivalent to C
% (e.g. via genus/2 and differentium/3)
% CDef = cdef(Genus,Diffs)
% Diffs = [R=To,...]
class_cdef(ID,cdef(G,Diffs)):-
        genus(ID,G),
        setof(R=To,differentium(ID,R,To),Diffs). 

cdef_placement(CDef,Equiv,NRParents,NRChildren,Redundant) :-
	solutions(Parent,subclassX(CDef,Parent),Parents),
	solutions(Parent,(member(Parent,Parents),subclassX(Parent,CDef)),Equiv),
	solutions(Child,subclassX(Child,CDef),Children),
	solutions(Child-Parent,(member(Parent,Parents),subclass(Child,Parent),member(Child,Children)),Redundant),
	solutions(Parent,(member(Parent,Parents),\+((subclass(P2,Parent),member(P2,Parents)))),NRParents),
	solutions(Child,(member(Child,Children),\+((subclass(Child,C2),member(C2,Children)))),NRChildren).


%%  equivalent_class(?Class1,?Class2) is nondet.
%    corresponds to owl:equivalentClass
:- extensional(equivalent_class/2).

%% subclassN(?Name,?SuperTermName) is nondet.
%  combines entity_label/2 and subclass/2
subclassN(Nc,Np):-
	(var(Nc)
        ->  class(IDp,Np),
	    subclass(ID,IDp),
	    class(ID,Nc)
        ;   class(ID,Nc),
	    subclass(ID,IDp),
	    class(IDp,Np)).
%% subclassTN(+Name,?SuperTermNameTransitive) is nondet.
% transitive version of subclassN/2
subclassTN(Nc,Np):-
	(var(Nc)
        ->  class(IDp,Np),
	    subclassT(ID,IDp),
	    class(ID,Nc)
        ;   class(ID,Nc),
	    subclassT(ID,IDp),
	    class(IDp,Np)).
%% subclassRTN(+Name,?SuperTermNameReflexiveTransitive) is nondet.
% reflexive transitive version of subclassN/2
subclassRTN(N,N).               % reflexive - X R X
subclassRTN(N,Np):-
        subclassTN(N,Np).

%%  restriction(?Class,?Relation,?ToClass) is nondet.
%  A relationship between two classes such that all instances of ?Class stand in ?Relation to some instance of ?ToClass
:- extensional(restriction/3).
%%  restriction(?Class,?Relation,?ToClass,?Arg3) is nondet.
% ternary relations
:- extensional(restriction/4).
%%  restriction(?Class,?Relation,?ToClass,?Arg3,?Arg4) is nondet.
% quaternary relations
:- extensional(restriction/5).

%%  cardinality_restriction(?Class,?Relation,?Card,?ToClass) is nondet.
%  A relationship between two classes such that all instances of ?Class stand in ?Relation to ?Card instance(s) of ?ToClass
:- extensional(cardinality_restriction/4).

%%  min_cardinality_restriction(?Class,?Relation,?Min_card,?ToClass) is nondet.
%  A relationship between two classes such that all instances of ?Class stand in ?Relation to at least ?Min instance(s) of ?ToClass
:- extensional(min_cardinality_restriction/4).

%%  max_cardinality_restriction(?Class,?Relation,?Max_card,?ToClass) is nondet.
%  A relationship between two classes such that all instances of ?Class stand in ?Relation to at most ?Max instance(s) of ?ToClass
:- extensional(max_cardinality_restriction/4).

%% range_cardinality_restriction(?A,?R,?Card,?B)
range_cardinality_restriction(A,R,Card,B):-
        cardinality_restriction(A,R,Card,B).
range_cardinality_restriction(A,R,Min-Max,B):-
        \+ cardinality_restriction(A,R,_,B),
        (   min_cardinality_restriction(A,R,Min,B)
        ->  true
        ;   Min=1),
        (   max_cardinality_restriction(A,R,Max,B)
        ->  true
        ;   Max=inf).

        

%% restrictionN(?Name,?RelationName,?SuperTermName) is nondet.
%  combines entity_label/2 and restriction/3
restrictionN(Nc,R,Np):-
	(var(Nc) 
        ->  class(IDp,Np),
	    restriction(ID,R,IDp),
	    class(ID,Nc)
        ;   class(ID,Nc),
	    restriction(ID,R,IDp),
	    class(IDp,Np)).

%% parentN(?Name,?ParentName,?SuperTermName) is nondet.
% combines entity_label/2 and parent/3
parentN(Nc,R,Np):-
	(var(Nc)
        ->  class(IDp,Np),
	    parent(ID,R,IDp),
	    class(ID,Nc)
        ;   class(ID,Nc),
	    parent(ID,R,IDp),
	    class(IDp,Np)).

% DEPRECATED - consider parent_over/3 or parentT/3
restrictionT(ID,[R],IDp):-
        restriction(ID,R,IDp),
        is_transitive(R).
restrictionT(ID,[R|RL],IDp):-
        (var(ID)
        ->  restriction(IDz,R,IDp),
            restrictionT(IDz,RL,ID)
        ;   restriction(ID,R,IDz),
            restrictionT(IDz,RL,IDp)),
        is_transitive(R).

%% child(?Parent,?Child) is nondet.
child(ID,IDc):- parent(IDc,ID).
%% child(?Parent,?Relation,?Child) is nondet.
child(ID,T,IDc):- parent(IDc,T,ID).

%% node_link(Node,Relation,Link) is nondet.
% true if parent/3 or inst_rel/3
node_link(X,Y,Z):- parent(X,Y,Z).
%node_link(X,Y,Z):- inst_rel(X,Y,Z). % now in parent/3
%node_link(X,instance_of,Y):- inst_of(X,Y).
%node_link(X,domain,Z):- property_domain(X,Z).
%node_link(X,range,Z):- property_range(X,Z).


%% parent(?Class,?SuperClass) is nondet.
% as parent/3
parent(ID,IDp):- parent(ID,_,IDp).

%% parent(?Class,?Relation,?ParentClass) is nondet.
%  combines subclass/2 and restriction/3
%  also genus/2 differentia/3
%  true if Relation=subclass and subclass(Class,ParentClass)
%    or if restriction(?Class,?Relation,?ParentClass)
parent(ID,subclass,IDp):-
	subclass(ID,IDp).
parent(ID,T,IDp):-
	restriction(ID,T,IDp).
parent(ID,T,IDp):-
	inst_rel(ID,T,IDp).
parent(ID,instance_of,IDp):-
	inst_of(ID,IDp).
% treat genus/diff as normal links [depr: use entailment_basic instead]
parent(ID,subclass,IDp):-
	genus(ID,IDp).
parent(ID,T,IDp):-
	differentium(ID,T,IDp).
parent(ID,equivalent,IDp):-
	equivalent_class(ID,IDp).


%% parent(?Class,+Relation,?ParentClass,+ViaRelation) is nondet.
% DEPRECATED?? consider parent_over/3
% true if Class is linked to ParentClass via either subclass, or
% ViaRelation, or ia one of the subproperties of ViaRelation
parent(ID,subclass,IDp,_):-
	subclass(ID,IDp).
parent(ID,T,IDp,T1):-
	restriction(ID,T,IDp),
        subclassRT(T,T1).

%% parentT(?Class,?ParentClass) is nondet.
% see parentT/3
parentT(ID,IDp):- parentT(ID,_,IDp).

%% parentT(?Class,?RelationList,?ParentClass) is nondet.
% transitive parent/3
parentT(ID,[ID-T],IDp):-
	parent(ID,T,IDp).
parentT(ID,[Link|TL],IDp):-
        (   var(ID)
        ->  parent(IDz,T,IDp),
            \+is_cyclic(T),
            Link=IDz-T,
            parentT(ID,TL,IDz)
        ;   parent(ID,T,IDz),
            \+is_cyclic(T),
            Link=ID-T,
            parentT(IDz,TL,IDp)).

%% parentT(?ID,+RelationList,?SuperClass,+ViaRelation) is nondet.
% DEPRECATED? consider parent_overT/3
% transitive parent relation via some relation
parentT(ID,TL,IDp,Via):-
        parentT1(ID,TL,IDp,Via),
        % must have at least one non-subclass step
        % is this useful?
        % the case where the last step is Via is now
        % covered by parent_overT
        member(T,TL),
        subclassRT(T,Via).

% TODO: relation subtyping
parentT1(ID,[T],IDp,Via):-
        parent(ID,T,IDp,Via).
parentT1(ID,[T|TL],IDp,Via):-
        (var(ID)
        ->  parent(IDz,T,IDp,Via),
            parentT1(ID,TL,IDz,Via)
        ;   parent(ID,T,IDz,Via),
            parentT1(IDz,TL,IDp,Via)).



parentRT(ID,[],ID,_).
parentRT(ID,TL,IDp,Via):- parentT(ID,TL,IDp,Via).

%% parentRT(?Class,?ParentClass) is nondet.
% reflexive transitive parent/2. See parentRT/3
parentRT(ID,IDp):- parentRT(ID,_,IDp).

%% parentRT(?Class,?Relation,?ParentClass) is nondet.
% reflexive transitive parent/3
parentRT(ID,[],ID).
parentRT(ID,TL,IDp):- parentT(ID,TL,IDp).

safe_parentT(ID,PID) :-
	safe_parentT(ID,_,PID).

safe_parentT(ID,R,PID) :-
	safe_parentT(ID,R,PID,[]).

safe_parentT(ID,[R],PID,_Visited) :-
	parent(ID,R,PID).
safe_parentT(ID,[R|Rs],PID,Visited) :-
	parent(ID,R,XID),
	\+ member(XID,Visited),
	safe_parentT(XID,Rs,PID,[XID|Visited]).

safe_parentRT(ID,PID) :-
	safe_parentRT(ID,_,PID).
safe_parentRT(ID,[],ID).
safe_parentRT(ID,Rs,PID) :-
	safe_parentT(ID,Rs,PID).

bf_parentRT(ID,PID) :-
	class(ID),
	ids_ancestors([ID],[],[],L),
	member(PID,L).
bf_parentRT(ID,ID) :-
	class(ID).
				%nonvar(ID).

ids_ancestors([ID|IDs],DoneIDs,Ancs,AncsFinal) :-
	setof(XID,parent(ID,XID),Parents),
	!,
	ord_union(Parents,IDs,U),
	sort(DoneIDs,DoneIDsSorted),
	ord_subtract(U,DoneIDsSorted,NextIDs),
	ord_union(Ancs,Parents,AncsNew),
	ids_ancestors(NextIDs,[ID|DoneIDsSorted],AncsNew,AncsFinal).
ids_ancestors([ID|IDs],DoneIDs,Ancs,AncsFinal) :-
	!,
	ids_ancestors(IDs,[ID|DoneIDs],Ancs,AncsFinal).
ids_ancestors([],_,Ancs,Ancs).


strict_subclass(A,B) :-
	subclass(A,B),
	\+((subclass(A,Z),Z\=B)).

class_single_inheritance_path(C,[]) :-
	class(C),
	\+ subclass(C,_).
class_single_inheritance_path(C,[D|Path]) :-
	setof(D,subclass(C,D),[D]),
	class_single_inheritance_path(D,Path).




%% parent_over(+R,?Class,?ParentClass) is nondet.
% as parent_over/4
parent_over(T,ID,PID):- parent_over(T,_,ID,PID).

%% parent_over(+R,?Via,?Class,?ParentClass) is nondet.
% true if ParentClass is a parent of Class by relation R
% uses subclass/2 rules:
%  X R Y if X subclassRT X1 and X1 R Y1 and Y1 subclassRT Y
%
%  notes: this will loop forever if there are subclass/2 cycles
parent_over(RL,Via,ID,PID):- 
        is_list(RL),
        !,
        member(R,RL),
        parent_over(R,Via,ID,PID).
parent_over(subclass,direct,ID,PID):- subclass(ID,PID).
parent_over(R,Via,ID,PID):-
        subclassRT(ID,XID),
        restriction(XID,R,XID2),
        subclassRT(XID2,PID),
        (   XID=ID
        ->  Via=direct
        ;   Via=subclassRT(XID)).
parent_over(instance_of,direct,ID,PID):-
        inst_of(ID,XID2),
        subclassRT(XID2,PID).
parent_over(R,direct,ID,PID):-
        inst_rel(ID,R,PID).
% transitive_over
parent_over(R,Via,ID,PID):-
        transitive_over(R,InhR),
        parent_over(InhR,ID,XID), % changed from poT/3, too slow, required?
        restriction(XID,R,PID),
        Via=parent(InhR,XID).

%% parent_overT(+R,?Class,?ParentClass) is nondet.
% transitive closure over parent_over/3
parent_overT(T,ID,PID):- parent_overT(T,ID,PID,[_|_]). % discount pure subclass paths

%% parent_overT(+R,?Via,?Class,?ParentClass) is nondet.
% transitive closure over parent_over/4
%parent_overT(T,ID,PID,Path):- parent_overT1(T,ID,PID,Path).
%parent_overT(T,ID,PID,Path):- parent_overT2(T,ID,PID,Path).
parent_overT(T,ID,PID,[PID]):- parent_over(T,ID,PID).
parent_overT(T,ID,PID,[IID|Path]):- parent_over(T,ID,IID),parent_overT(T,IID,PID,Path).
%parent_overT2(T,ID,PID,Path):- subclassRT(ID,IID),parent_overT1(T,IID,XID,Path),subclassRT(XID,PID).

% sub-relations
parent_overT(R,ID,PID,Path):-
        nonvar(R),
        property(R),
        subclassT(AR,R),
        %writeln(testing-AR-R-ID-PID),
        parent_overT(AR,ID,PID,Path).


%% parent_overT(+R,?Via,?Class,?ParentClass) is nondet.
% reflexive transitive closure over parent_over/4
parent_overRT(_,ID,ID).
parent_overRT(T,ID,PID):- parent_overT(T,ID,PID).

%% parent_over_nr(+R,?ID,?PID) is nondet.
% as parent_over_nr/4
parent_over_nr(R,ID,PID):- parent_over_nr(R,_,ID,PID).

%% parent_over_nr(+R,?Via,?ID,?PID) is nondet.
% nonredundant version of parent_over/4
%
%  eliminates redundancy; for example, if X part_of organelle, and
%nucleus is_a+ organelle, and X can be inferred to be part_of a
%nucleus, then X part_of organelle is redundant
%
% ==
%  X --[p]--> nucleus --[is_a]--> organelle
%  |                                  ^
%  |                                  |
%  +----------[p]---------------------+
% ==
parent_over_nr(R,Via,ID,PID):-
        parent_over(R,Via,ID,PID),
        \+ (   parent_over(R,ID,IID),
               (   parent_over(R,IID,PID)
               ;   subclassT(IID,PID))),
        \+ (subclassT(ID,IID),parent_over(R,IID,PID)).

%%  belongs(?Entity,?Ontology) is nondet.
%  true if Entity belongs to Ontology - equivalent to entity_resource/2
%  may be deprecated in future - consider entity_resource/2
:- multifile belongs/2.
belongs(X,To):- entity_resource(X,To),is_class_or_property_or_inst(X).
belongs(X,To):- entity_resource(X,To),entity_obsolete(X,_).

%% is_class_or_property_or_inst(+E) is det.
is_class_or_property_or_inst(X):- class(X),!.
is_class_or_property_or_inst(X):- property(X),!.
is_class_or_property_or_inst(X):- inst(X),!.

%%  synonym(?Class,?Scope,?Synonym) is nondet.
%   may be deprecated in future - consider entity_synonym/2 and entity_synonym_scope/3
% Scope='' if no scope is specified
:- multifile synonym/3.
synonym(Class,Scope,Synonym):- entity_synonym_scope(Class,Synonym,Scope).
synonym(Class,'',Synonym):- entity_synonym(Class,Synonym), \+ entity_synonym_scope(Class,Synonym,_).

%%  class_xref(?Class,?DBXref) is nondet.
:- multifile class_xref/2.
class_xref(Class,Xref):- entity_xref(Class,Xref),class(Class).

%%  def_xref(?Class,?DBXref) is nondet.
%   true if DBXRef is the source of the def/2 fact for Class
%   this is currently not defined in terms of entity_xref/2
:- extensional(def_xref/2).

%%  class_comment(?Class,?Comment) is nondet.
%  Combines class/1 and entity_comment/2
%  holds when class(Class) and entity_comment(Class,Comment)
:- multifile class_comment/2.
class_comment(Class,Comment):- entity_comment(Class,Comment).  %,class(Class).


%%  def(?Class,?DefinitionText) is nondet.
%   true if DefinitionText is the natural language definition of Class or Relation
%%  def(+Class,?DefinitionText) is semidet
:- extensional(def/2).
% consider: using entity_description/2?
% consider: using entity_definition/2?

%%  disjoint_from(?Class,?DisjointClass)
%   true if there is nothing that instantiates both Class and DisjointClass
:- extensional(disjoint_from/2).

%%  class_disjoint_union_list(?Class,?JEPDClassList) is nondet.
% inferred from disjoint_from/2 and class_union_element/2
class_disjoint_union_list(Class,Us):-
        setof(U,class_union_element(Class,U),Us),
        forall(member(C1,Us),
               forall((member(C2,Us),C1\=C2),
                      disjoint_from(C1,C2))).

disjoint_from_violation(P1,P2,C):-
        disjoint_from(P1,P2),
        subclassT(C,P1),
        subclassT(C,P2).

% this is slow - recommended use ontol_reasoner first..
disjoint_over_violation(DR,X,Y,A):-
        disjoint_over(DR,R),
        (   restriction(X,DR,Y)
        ;   restriction(Y,DR,X)),
        debug(ontol,'testing disjoint_over ~w :: ~w ~w ~w',[R,X,DR,Y]),
        parent_overRT(R,A,X),
        parent_overRT(R,A,Y).


%%  genus(?Class,?Genus) is nondet.
%    A logical definition of Class refines Genus
%    via some discriminating characteristic (differentium/3);
%    e.g. a Class is a Genus *which* Differentia hold
:- extensional(genus/2).

%%  differentium(?Class,?Rel,?ToClass) is nondet.
%    Must be used in conjunction with genus/2
:- extensional(differentium/3).

%%  class_union_element(?Class,?Element) is nondet.
%    for covering axioms. same as owl:unionOf
:- extensional(class_union_element/2).

%%  class_intersection_element(?Class,?Element) is nondet.
%    for definitions. same as owl:intersectionOf
:- extensional(class_intersection_element/2).

%%  property_intersection_element(?Property,?Element) is nondet.
%    we write the relation intersection p^q as two facts
%      property_intersection_element('p^q',p).
%      property_intersection_element('p^q',q).
%    no equivalent in OWL or OWL2
:- extensional(property_intersection_element/2).

%% property_intersection_elements(?Property,?Elements) is semidet
property_intersection_elements(R,L):- setof(X,property_intersection_element(R,X),L).

%%  property_union_element(?Property,?Element) is nondet.
%    we write the relation union p^q as two facts
%      property_union_element('p^q',p).
%      property_union_element('p^q',q).
%    no equivalent in OWL or OWL2
:- extensional(property_union_element/2).

%% property_union_elements(?Property,?Elements) is semidet
property_union_elements(R,L):- setof(X,property_union_element(R,X),L).


%**************************************
%*              RELATIONS                *
%***************************************

%% relation(?Rel)
% deprecated: disjunction of property/1 and slot/1
%  holds if Rel is a property or slot
relation(X):- property(X).

%%  property(?Relation) is nondet.
%    holds when Relation is the ID of an entity which defines a relationship
%    which holds between two classes (arg2 of restriction/3) or
%    between two instance (arg2 of inst_rel/3)
%    equivalent to owl:ObjectProperty
:- extensional(property/1).

%% property(?Relation,?Name) is nondet.
%  combines property/1 and entity_label/2
%  holds when property(?Relation) and entity_label(?Relation,?Name)
%  see entity_label/2
:- multifile property/2.
property(P,Name):- entity_label(P,Name),property(P).

%%  property_domain(?Relation,?Class) is nondet.
% all instances that are the subject of Relation instantiate Class
:- extensional(property_domain/2).

%%  property_range(?Relation,?Class) is nondet.
% all instances that are the object of Relation instantiate Class
:- extensional(property_range/2).

%% complement_of(?NegRel,?Rel)
:- extensional(complement_of/2).

:- extensional(holds_temporally_between/3).
:- extensional(holds_atemporally_between/3).

%%  lexical_category(?Class,?Type) is nondet.
% DEPRECATED
:- extensional(lexical_category/2).

%%  is_anonymous(?Entity) is nondet.
% true if Entity is unnamed
% equivalent to bNode in RDF
:- extensional(is_anonymous/1).

%%  is_unsatisfiable(?Class) is nondet.
% true if Class is defined in such a way it can never be instantiated;
% for example, being a subclass of two disjoint classes
:- extensional(is_unsatisfiable/1).

%%  inverse_of(?Relation,?Inverse) is nondet.
% true if Inverse is the inverse of Relation, such that the inverse is held between instances
% formally: inst_rel(X,R,Y),inverse(R,R1) => inst_rel(Y,R1,X)
:- extensional(inverse_of/2).

%%  inverse_of_on_instance_level(?Relation,?Inverse) is nondet.
% true if all_some(Relation,InstRelation),inverse_of(InstRelation,InstRelationInv),all_some(InstRelationInv,Inverse)
% (always asserted)
:- extensional(inverse_of_on_instance_level/2).

%%  class_level_inverse_of(?Relation,?Inverse) is nondet.
% DEPRECATED: just use inverse_of
% true if Inverse is the inverse of Relation, such that the inverse is held between classes
% formally: restriction(X,R,Y),class_inverse(R,R1) => restriction(Y,R1,X)
:- extensional(class_level_inverse_of/2).

%%  holds_bidirectionally_for(?IntegralRelation,?Relation) is nondet.
% holds_bidirectionally_for(SR,R), X SR Y, R inverse_of RI => X R Y and Y RI X
% DEPRECATED. Use holds_bidirectionally_for/3
:- extensional(holds_bidirectionally_for/2).

%%  holds_bidirectionally_for(?IntegralRelation,?Relation,?InverseRelation) is nondet.
% holds_bidirectionally_for(SR,R,Inv), X SR Y => X R Y and Y Inv X
:- extensional(holds_bidirectionally_for/3).

%%  is_metadata_tag(?Relation) is nondet.
% does this relation only hold at the class metadata level?
% same as owl:AnnotationProperty
:- extensional(is_metadata_tag/1).

%%  is_class_level(?Relation) is nondet.
% Relation only holds between classes: also is non-inheritable
:- extensional(is_class_level/1).

%%  is_transitive(?Relation) is nondet.
% formally: X R Y, Y R Z, is_transitive(R) => X R Z
:- extensional(is_transitive/1).

%% metaproperty(?Relation,?Property) is nondet
% reified unary relation predicates.
% @param Property one of is_transitive/1, is_symmetric/1, is_cyclic/1, is_asymmetric/1, is_anti_symmetric/1, is_functional/1, is_inverse_functional/1
metaproperty(X,is_transitive):- is_transitive(X).
metaproperty(X,is_symmetric):- is_symmetric(X).
metaproperty(X,is_anti_symmetric):- is_anti_symmetric(X).
metaproperty(X,is_functional):- is_functional(X).
metaproperty(X,is_inverse_functional):- is_inverse_functional(X).
metaproperty(X,is_cyclic):- is_cyclic(X).
metaproperty(X,is_reflexive):- is_reflexive(X).


%%  transitive_over(?Relation,?Over) is nondet.
% true if Relation is transitive over Over
% formally: X R Y, Y R1 Z, transitive_over(R,R1) => X R Z
:- extensional(transitive_over/2).

%%  holds_over_chain(?Relation,?RelationList) is nondet.
% true if Relation is transitive over the chain of relations in RelationList
% e.g. if holds_over_chain(R,[R1,R2, ..., Rn]) then
% X R Y <= X R1 Z1, Z1 R2 Z2, ..., Zn-1 Rn Y
:- extensional(holds_over_chain/2).

%%  equivalent_to_chain(?Relation,?RelationList) is nondet.
% true if Relation is equivalent the chain of relations in RelationList
% e.g. if equivalent_to_chain(R,[R1,R2, ..., Rn]) then
% X R Y <=> X R1 Z1, Z1 R2 Z2, ..., Zn-1 Rn Y
:- extensional(equivalent_to_chain/2).

%%  is_symmetric(?Relation) is semidet
% class level. formally: X R Y,is_symmetric(R) => Y R X, X and Y are classes
:- extensional(is_symmetric/1).

%%  is_asymmetric(?Relation) is semidet
:- extensional(is_asymmetric/1).

%%  is_symmetric_on_instance_level(?Relation) is semindet
% formally: X R Y,is_symmetric(R) => Y R X, X and Y are instances
:- extensional(is_symmetric_on_instance_level/1).

%%  is_reflexive(?Relation) is semidet
% formally: forall(X), is_reflexive(R) => X R X
:- extensional(is_reflexive/1).

%%  is_anti_symmetric(?Relation) is semidet
:- extensional(is_anti_symmetric/1).

%%  is_cyclic(?Relation) is semidet
:- extensional(is_cyclic/1).

%%  is_functional(?Relation) is semidet
:- extensional(is_functional/1).

%%  is_inverse_functional(?Relation) is semidet
:- extensional(is_inverse_functional/1).

%%  is_nondangling(?Relation) is semidet
is_nondangling(X):- class(X).
is_nondangling(X):- property(X).
is_nondangling(X):- inst(X).

%%  is_proper(?Relation) is nondet.
:- extensional(is_proper/1).

%% transitive_form_of(?TransitiveRel,?Rel) is nondet
% strictly required? We can make the Rel a subproperty of TransitiveRel instead, but this is less intuitive
:- extensional(transitive_form_of/2).

%% proper_form_of(?ProperRel,?Rel) is nondet
:- extensional(proper_form_of/2).

%% cyclic_form_of(?CyclicRel,?Rel) is nondet
:- extensional(cyclic_form_of/2).

%% cyclic_over(?CyclicRel,?Rel) is nondet
:- extensional(cyclic_over/2).

%% reflexive_over(?RRel,?Rel) is nondet
:- extensional(reflexive_over/2).

%% directed_simple_path_over(?CyclicRel,?Rel) is nondet
:- extensional(directed_simple_path_over/2).

%% directed_path_over(?CyclicRel,?Rel) is nondet
:- extensional(directed_path_over/2).


%%  id_axiom(?Relation,?Axiom) is nondet.
% Axiom can be any term
:- extensional(id_axiom/2).

%%  all_some(?Relation) is nondet.
% DEPRECATED: see below - but used in ontol_reasoner
%:- extensional(all_some/1).
all_some(Rel):- property_relationship(Rel,all_some,_).
all_some(Rel):-                 % all_some by default
        property(Rel),
        \+ is_class_level(Rel),
        \+ disjoint_over(Rel,_),
        \+ is_metadata_tag(Rel),
        \+ complement_of(Rel,_).

%% disjoint_over(?Rel,?RelOver)
:- extensional(disjoint_over/2).

%disjoint_over(Rel,Over):-
%        property_relationship(Rel,disjoint_over,Over).

%% property_relationship(?P,?Rel,?Filler) is notnet
% @param ?Rel = all_some_all_times ; all_some_some_times ; all_some_tr
:- extensional(property_relationship/3).

%%  holds_for_all_times(?Relation) is nondet.
:- extensional(holds_for_all_times/1).

%%  obsolete(?Class,?Name,?Ontology) is nondet.
% DEPRECATED: combines entity_label/2, entity_obsolete/1 and belongs/2
:- multifile obsolete/3.
obsolete(Class,Name,Ontology):- entity_label(Class,Name),entity_obsolete(Class,class),entity_resource(Class,Ontology).

%%  obsolete_class(?Class,?Name) is nondet.
% DEPRECATED combines entity_label/2, entity_obsolete/1
:- multifile obsolete_class/2.
obsolete_class(Class,Name):- entity_label(Class,Name),entity_obsolete(Class,class).

%%  idspace(?IDSpace,?IDSpaceLong) is nondet.
% DPRECATED: use idspace_uri/2 instead
:- multifile idspace/2.
idspace(S,URI):- metadata_db:idspace_uri(S,URI).

%**************************************
%*              INSTANCES                *
%***************************************

%%  inst(?Instance) is nondet.
% true if Instance is declared to be an instance of some class
:- extensional(inst/1).

%% inst(?Instance,?Name) is nondet.
%  combines inst/1 and entity_label/2
%  holds when inst(?Instance) and entity_label(?Instance,?Name)
:- multifile inst/2.
inst(I,Name):- entity_label(I,Name),inst(I).

%%  inst_of(?Instance,?Class) is nondet.
% true if Instance is a direct instaniation of Class
% - this represents a 'timeless' instantiation, such as
%   set membership or owl/DL.
%   See also inst_of/3 for time-indexed instantiation
:- extensional(inst_of/2).


%%  inst_of_at(?Instance,?Class,?Time) is nondet.
% true if Instance is a direct instaniation of Class at Time
% time-indexed version of inst_of/2
% see also inst_rel/4
:- extensional(inst_of_at/3).


%%  inst_ofRT(?Instance,?Class) is nondet.
% true if Instance is an instance of Class, taking into account transitivity
% combines inst_of/2 and subclassRT/2
inst_ofRT(Inst,Class):-
        subclassRT(SubClass,Class),
        inst_of(Inst,SubClass).


%%  inst_rel(?Instance,?Relation,?ToInstance) is nondet.
% true if Instance stands in Relation to ToInstance
% - this represents a 'timeless' relationship
%   See also inst_rel/4 to time-indexed relations
:- extensional(inst_rel/3).

:- extensional(inst_rel/4).
:- extensional(inst_rel/5).
:- extensional(inst_rel/6).
:- extensional(inst_rel/7).

%%  inst_rel_at(?Instance,?Relation,?ToInstance,?Time) is nondet.
% true if Instance stands in Relation to ToInstance at/during Time.
% time is not explicitly modeled in the ontology language; Time
% will be an instance of an appropriate class in an upper ontology
% (eg BFO or owl-time).
% deprecated: use inst_rel/4 instead
:- extensional(inst_rel_at/4).

%% inst_rel_type(?I,?Rel,?Type)
% true if I is related by Rel to some instance of type Type
inst_rel_type(I,Rel,Type):- inst_rel(I,Rel,I2),inst_of(I2,Type).


%%  inst_rel_anon(?Instance,?Relation,?Class) is nondet.
% true if Instance stands in Relation to some unnamed instance of Class
:- extensional(inst_rel_anon/3).

%%  inst_sv(?Instance,?Slot,?Value,?DataType) is nondet.
% true if Instance has a Slot with Value, where Value is of DataType
:- extensional(inst_sv/4).

%% inst_sv(?Inst,?Relation,?Relatum) is nondet.
%  disjunction of inst_sv/4 and inst_rel/3
%  true if inst_sv(Inst,Relation,Reulatum) or inst_rel(Inst,Relation,Relatum)
inst_sv(ID,S,V):-
        inst_sv(ID,S,V,_).
inst_sv(ID,S,PID):-
        inst_rel(ID,S,PID).

%% class_instrule(?Class,?HeadVars,?PrologRuleBody)
%
:- extensional(class_instrule/3).

%% class_reified_rulebody(?Class,HeadVars,?HeadConjTerms)
% reified form of class_instrule/3
:- extensional(class_reified_rulebody/3).


%%  reification(?Statement,?Term) is nondet.
%   true if Statement identifies the fact Term
%   currently Term must be restriction/3 or inst_rel/3
:- extensional(reification/2).

%% entailed_by(?Statement,?Rule) is nondet.
% true if Statement is entailed (logically inferred) by Rule
% Expression may be 'true' if deduction provenance is not maintained
:- extensional(entailed_by/2).

%% entailed_by(?Statement,?Rule,?Expression) is nondet.
:- extensional(entailed_by/3).

%% logicalformula(?ID,?FormulaAtom,?Language) is nondet.
:- extensional(logicalformula/3).

%% merge_class(+Src,+Tgt) is det
merge_class(Src,Tgt) :-
	merge_class(Src,Tgt,[merge(all)]).
			     

%% merge_class(+Src,+Tgt,+Opts) is det
merge_class(Src,Tgt,Opts) :-
	debug(merge,'merging ~w -> ~w',[Src,Tgt]),
	merge_class_axiom(subclass(Src,X),subclass(Tgt,X),Opts),
	merge_class_axiom(restriction(Src,R,X),restriction(Tgt,R,X),Opts),
	merge_class_axiom(genus(Src,X),genus(Tgt,X),Opts),
	merge_class_axiom(differentium(Src,R,X),differentium(Tgt,R,X),Opts),
	merge_class_axiom(entity_synonym_scope(Src,R,X),entity_synonym_scope(Tgt,R,X),Opts),
	retractall(class(Src)),
	assert(metadata_db:entity_alternate_identifier(Tgt,Src)).

merge_class_axiom(Src,Tgt,Opts) :-
	Src =.. [P|_],
	(   member(merge(P),Opts)
	;   member(merge(all),Opts)),
	!,
	forall((Src,\+invalid(Tgt)),
	       assert(Tgt)).
merge_class_axiom(_,_,_).

invalid(subclass(X,X)).
invalid(restriction(X,_,X)).


	
%% class_by_name_or_synonym(+N,?ID) is nondet.
% DEPRECATED. index instead
class_by_name_or_synonym(N,ID):-
        class(ID,N).
class_by_name_or_synonym(N,ID):-
        synonym(ID,_,N).


%% class_label(?Class,?Label,?Type)
% true if Class has name or synonym Label (case-insensitive)
%  Label='exact' for names and exact synonyms; otherwise synonym type
% see class/2 and synonym/3
class_label(ID,N,exact):-  class(ID,N).
class_label(ID,N,T):-
        synonym(ID,T1,N),  %downcase_atom(N1,N),
        (   T1=''
        ->  T=related
        ;   T=T1).

class_label_exact(ID,N):-  class_label(ID,N,exact).

%% id_name(Entity,Name) is nondet.
% DEPRECATED
id_name(ID,N):- class(ID,N).
id_name(ID,N):- property(ID,N).
id_name(ID,N):- inst(ID,N).


% TODO
dbmeta:resolve_query(synonym(S),class(ID)):-
        (   class(ID,S)
        ;
            synonym(ID,_,S)).



subpathL(P,IDp,L,P1):-
        member(P1/IDp,L),
        P1\=P,
        subpath(P,P1).

subpath([],_).
subpath([T|P1],[T|P2]):-
        !,
        subpath(P1,P2).
subpath(P1,[_|P2]):-
        !,
        subpath(P1,P2).

cpath(Pi,Po):-
        cpath(Pi,Po,[],null).
cpath([],X,X,_).
cpath([subclass|TL],P,Pb,Last):-
        !,
        cpath(TL,P,Pb,Last).
cpath([T|TL],P,Pb,Last):-
        (T=Last
        ->  cpath(TL,P,Pb,Last)
        ;   cpath(TL,P,[T|Pb],T)).


%% referenced_id(?ID,?RefID)
%
%  ID references RefID if RefID appears as object of
%subclass/restriction/intersection relation
referenced_id(ID,RefID):-     restriction(ID,_,RefID).
referenced_id(ID,RefID):-     subclass(ID,RefID).
referenced_id(ID,RefID):-     differentium(ID,_,RefID).
referenced_id(ID,RefID):-     genus(ID,RefID).


% -----------------------------------
% BASIC REASONING
% -----------------------------------
% these parts do not rely on classdefs;
% simple querying based purely on subclass/2 and restriction/3

%% topclass(?Class) is nondet.
%  true if Class is top of subclass hierarchy
topclass(C):-
	class(C),
        \+subclass(C,_).

%% topclass(?Class,?Ontology)
% true if Class is top of subclass hierarchy within Ontology
topclass(C,O):-
        belongs(C,O),
	\+ (subclass(C,PID),belongs(PID,O)).

%% noparent(?Class)
% true if Class is top of union of subclass and restriction hierarchy
noparent(C):-
	class(C),
        \+ ((parent(C,P),class(P))).

%% noparent_and_haschild(?Class)
% true if noparent and not nochild
noparent_and_haschild(C):-
        class(C),
        noparent(C),
        \+ nochild(C).

%% noparent(?Class,?Ontology)
% true if Class is top of union of subclass and restriction hierarchy within Ontology
noparent(C,O):-
        belongs(C,O),
        \+((parent(C,PID),belongs(PID,O))).

%% noparent_and_haschild(?Class,?Ontology)
% true if noparent and not nochild
noparent_and_haschild(C,O):-
        noparent(C,O),
        \+ \+ parent(_,C).

%% noparent_by_type(?Relation,?Class)
% true if Class is a class with no parents over Relation
% uses parent_over/3
noparent_by_type(T,C):-
        class(C),
        \+ parent_over(T,C,_).

%% nochild(?Class)
% true if Class is bottom of union of subclass and restriction hierarchy
nochild(C):-
	class(C),
	\+(parent(_,C)).

%% nochild(?Class,?Ontology)
% true if Class is bottom of union of subclass and restriction hierarchy within Ontologyy
nochild(C,O):-
        belongs(C,O),
	\+((parent(CID,C),belongs(CID,O))).

%% redundant_subclass(?Class,?Parent,?Intermediate) is nondet.
%  true if Class is direct subclass/2 of Parent, and Class is a subclassT/2 of Intermediate which is a subclassT/2 of Parent
redundant_subclass(ID,IDp,IDz):-
        class(ID),
        \+ subclass_cycle(ID,_),
	subclass(ID,IDp),
	subclassT(ID,IDz),
	subclassT(IDz,IDp).

%% redundant_parent(?ID,?RelationshipType,?IDp,?IDz,?Path) is nondet.
redundant_parent(ID,T,PID,ZID,TL):-
        \+ parent_cycle(ID,_),
        restriction(ID,T,PID),
        subclassT(ID,ZID),
        parentT(ZID,TL,PID,T).

%% multiple_parent(?ID,?T,?PID1,?PID2)
%  finds classes with multiple parents of the same time
%
%  takes into account the rule:
%  
%  if   X is_a* Y
%  and  Y part_of Z
%  then X part_of Z
%
%  (substitute part_of for any other relation T here)
%
%  PID1 must be a direct part_of ID, but PID2 can be an indirect part
%
%  does not report if PID1 is_a* PID2 (because they are not distinct parts)
%  
%  Note that this will unify with duplicate results as PID1 and PID2
%are symmeryical. When PID1 and PID2 are unground, you can de-duplicate
%like this:
%
%  ==
%  multiple_parent(ID,part_of,PID1,PID2),
%  PID1@<PID2.
%  ==
multiple_parent(ID,T,PID1,PID2):-
        multiple_parent(ID,T,PID1,PID2,_).

%% multiple_parent(?ID,?T,?PID1,?PID2,?ViaID)
%   As multiple_parent/4 but also provides the ID of the class
%ViaID such that PID2 is a direct part_of ViaID (and ViaID is a
%superclass* of ID)
%
%  PID1 is always a direct part_of ID
%
%  (substitute part_of for any other relation T here)
multiple_parent(ID,T,PID1,PID2,ViaID):-
        restriction(ID,T,PID1),
        subclassRT(ID,ViaID),
        restriction(ViaID,T,PID2),
        PID1\=PID2,
        \+ parentRT(PID1,PID2).
        % GO is missing is_a parents, so we need the above hack
        %\+ subclassRT(PID1,PID2).

%% subclass_underlap(?C,?P1,?P2) is nondet.
% C is_a P1 and C is_a P2
% P1 and P2 are non-disjoint because o C
subclass_underlap(C,P1,P2):-
        subclassT(C,P1),
        subclassT(C,P2),
        P1\=P2.


%% subclass_cycle(+ID,?Path) is nondet.
%  check for cyclical paths, going up from ID
% 
% subclass cycles are always illegal, and will cause
% non-terminating behaviour
subclass_cycle(ID,P):-
	subclass_cycle(ID,P,[ID]).

%% subclass_cycle(+ID,?Path,+InitPath) is nondet.
subclass_cycle(ID,PathWithCycle,P):-
	subclass(ID,SuperID),
	(   member(SuperID,P)
        ->  PathWithCycle=[SuperID|P]
        ;   subclass_cycle(SuperID,PathWithCycle,[SuperID|P])).

%% parent_cycle(+ID,?Path) is nondet.
%  check for cyclical paths, going up from ID
% a parent is defined by subclass/2 or restriction/3
% 
% parent cycles will cause non-terminating behavior
% example: parent_cycle(X,Y1),!,reverse(Y1,Y),writeln(Y),member(A-R-B,Y),class(A,AN),class(B,BN),writeln(A-AN-R-B-BN),fail.
parent_cycle(ID,P):-
	parent_cycle(ID,P,[]).

%% parent_cycle(+ID,?Path,+InitPath) is nondet.
parent_cycle(ID,[SuperID-R2-S2,ID-R-SuperID|P1],P):-
	parent(ID,R,SuperID),
	member(SuperID-_-_,P),!,
        append(P1,[SuperID-R2-S2|_],P).

parent_cycle(ID,X,P):-
	parent(ID,R,SuperID),
	parent_cycle(SuperID,X,[ID-R-SuperID|P]).

abduced_link(subclass(Yc,Yp)):-
        subclassT(Xc,Xp),
        genus(Xc,G),
        genus(Xp,G),
        differentium(Xc,R,Yc),
        differentium(Xp,R,Yp),
        \+ parentRT(Yc,Yp).

abduced_link_nr(subclass(A,B)):-
        abduced_link(subclass(A,B)),
        \+ (( abduced_link(subclass(A,C)),
              abduced_link(subclass(C,B)))).

remove_dangling_facts :-
	dangling_fact(F),
	retractall(F),
	fail.
remove_dangling_facts.

dangling_fact(F) :-
	F=subclass(A,B),
	F,
	\+ ((is_nondangling(A),
	     is_nondangling(B))).
dangling_fact(F) :-
	F=disjoint_from(A,B),
	F,
	\+ ((is_nondangling(A),
	     is_nondangling(B))).
dangling_fact(F) :-
	F=restriction(A,R,B),
	F,
	\+ ((is_nondangling(A),
	     is_nondangling(R),
	     is_nondangling(B))).
dangling_fact(F) :-
	class_cdef(C,cdef(G,DL)),
	\+ ((is_nondangling(G),
	     forall(member(R=X,DL),
		    (	is_nondangling(R),
			is_nondangling(X))))),
	class_cdef_fact(C,cdef(G,DL),F).

class_cdef_fact(C,cdef(G,_),genus(C,G)).
class_cdef_fact(C,cdef(_,DL),differentium(C,R,X)) :-
	member(R=X,DL).


	
	

% experimental stuff from here on

dangling_class(ID):-
        (parent(ID,_) ; parent(_,ID)),
        (not(class(ID)) ; not(belongs(ID,_))).

classdef_parent(Class,PClass):-
        not(var(Class)),
        not(var(PClass)),
        ((cdef(ID,Class),cdef(PID,PClass))
        ->  parentRT(ID,PID)
        ;   classdef_parent_c(Class,PClass)).
classdef_parent_c(Class,PClass):-
	%format('~n~w~n~w~n~n',[Class,PClass]),
	not(var(Class)),
	not(var(PClass)),
	Class=intersection(GenusID,DiffL),
	PClass=intersection(PGenusID,PDiffL),
	select(RelType:ToClass,DiffL,DiffL2),
	(select(RelType:PToClass,PDiffL,PDiffL2) % unify reltype
	->  classdef_parent(ToClass,PToClass) % parentD
	;   PDiffL2=PDiffL),    % parent of top
	classdef_parent(intersection(GenusID,DiffL2),
			intersection(PGenusID,PDiffL2)).


class_id_or_label(ID,N):- (class(ID,N)-> true ; N=ID).
cdef_label(cdef(G,Diffs),Label):-
        class_id_or_label(G,GN),
        findall(DiffN,
                (   member(R=To,Diffs),
                    class_id_or_label(R,RN),
                    class_id_or_label(To,ToN),
                    sformat(DiffN,'~w ~w',[RN,ToN])),
                DiffNs),
        concat_atom(DiffNs,' and ',DiffAtom),
        sformat(Label,'~w that ~w',[GN,DiffAtom]).



% private
%  TODO ALSO returns subclasses; fix this
cdef(ID,intersection(GenusID,DiffL)):-
        (var(ID)
        ->  (DiffL=[]
            ->  GenusID=ID
            ;   genus(ID,GenusID),
                forall(member(R:ToClass,DiffL),
                       (   cdef(ToID,ToClass), % (?,+)
                           differentium(ID,R,ToID))))
        ;   (genus(ID,GenusID)
            ->  setof(R:ToClass,
                      (differentium(ID,R,ToID),cdef(ToID,ToClass)),
                      DiffL)
            ;   GenusID=ID,DiffL=[])),
        !.

% (+,?) eg for queries
classdef_subsumes_id(PClass,ID):-
        not(var(PClass)),
        class(ID),            % make sure unified
        (cdef(PID,PClass)
        ->  subclassRT(ID,PID)
        ;   classdef_subsumes_id_c(PClass,ID)).

% PClass is NOT extant
classdef_subsumes_id_c(PClass,ID):-
	PClass=intersection(PGenusID,PDiffL),
	%Class=intersection(GenusID,DiffL),
	genus(ID,GenusID),
	subclassRT(GenusID,PGenusID),
	forall(member(R:PToClass,PDiffL),
	       (differentium(ID,R,ToID),
		classdef_subsumes_id(PToClass,ToID))).

% should this stay here or go to ontol_manifest_names_from_genus_differentia?
name_composed_type(Genus^Diff,Name):-
        !,
        name_composed_type(Genus,GenusName),
        name_composed_type(Diff,Which),
        concat_atom([GenusName,' that ',Which],Name).
name_composed_type(Rel=To,N):-
        !,
        name_composed_type(Rel,RelN),
        name_composed_type(To,ToN),
        concat_atom([RelN,' ',ToN],N).
name_composed_type([],''):- !.
name_composed_type([D],N):- !, name_composed_type(D,N).
name_composed_type([D|Ds],N):-
        !,
        name_composed_type(D,N1),
        name_composed_type(Ds,N2),
        concat_atom([N1,' and ',N2],N).
name_composed_type(C,N):- class(C,N),!.
name_composed_type(C,N):- property(C,N),!.
name_composed_type(X,X).

% -------------------- SEGMENTATION --------------------

%% ontology_segment(+InNodes,?OutNodes,?Opts)
ontology_segment(InNodes,OutNodes,Opts):-
        ontology_segment(InNodes,_,OutNodes,Opts).
%% ontology_segment(+InNodes,?Edges,?OutNodes,?Opts)
ontology_segment(InNodes,Edges,OutNodes,Opts):-
        debug(ontol,'Segmenting: ~w Opts: ~w',[InNodes,Opts]),
        (   member(relations(Rels),Opts),
            Rels\=[]
        ->  true
        ;   member(exclude_relations(ExcRels),Opts)
        ->  solutions(Rel,(parent(_,Rel,_),\+member(Rel,ExcRels)),Rels)
        ;   Rels=[]),
        relation_closure_predicate(Rels,Pred,Opts), % unify Pred with Goal for matching node to parents
        debug(ontol,'Closure pred: ~w ',[Pred]),
        (   member(maxdown(MaxDown),Opts)
        ->  true
        ;   MaxDown=0),
        (   member(siblings(1),Opts)
        ->  solutions(PNode,(member(Node,InNodes),call(Pred,Node,PNode)),InNodesPlus),
            Delta=down(1)
        ;   member(maxup(MaxUp),Opts),nonvar(MaxUp)
        ->  Delta=up(MaxUp)
        ;   InNodesPlus=InNodes,
            Delta=down(MaxDown)),
        debug(ontol,'Will use ~w for graph traversal. delta=~w',[Pred,Delta]),
        closure_to_edgelist_delta(Pred,InNodesPlus,Edges1,Delta,0), % todo IsReverse
        debug(ontol_segment,'collapsing: ~w',[Edges]),
        % todo: optimize this
        (   member(collapse_relation(CR),Opts)
        ->  collapse_edgelist_via_pred(Edges1,Edges,collapse_relation(CR)) % todo
        ;   member(collapse_predicate(CP),Opts)
        ->  collapse_edgelist_via_pred(Edges1,Edges,CP)
        ;   Edges=Edges1),
        debug(ontol_segment,'Converting edges to trees: ~w',[Edges]),
        edgelist_to_trees(Edges,Trees),
        debug(ontol_segment,'Converted edges to trees: ~w',[Trees]),
        solutions(OutNode,
                  (   member(Tree,Trees),
                      tree_node_ids(Tree,OutNodes1),
                      member(OutNode,OutNodes1)),
                  OutNodes).

collapse_edgelist_via_pred(L,L,fail):- !.
collapse_edgelist_via_pred(EdgesIn,EdgesOut,Pred):-
        findall(N,(   member(edge(N,_,_),EdgesIn)
                  ;   member(edge(_,N,_),EdgesIn)),Nodes),
        findall(N,(member(N,Nodes),user:call(Pred,N,EdgesIn)),NodesToGo),
        collapse_edgelist(EdgesIn,EdgesOut,NodesToGo,EdgesIn).

/*
xxcollapse_edgelist(EL,EL2,[N|NodesToGo],ELO):-
        findall(edge(X,Z,R2),member(edge(Y,Z,R2),EL),ELJ),
        findall(E2,(member(E2,EL),E2\=edge(Y,_,_)),ELRest),
        foo.

        
collapse_edgelist([E|EL],EL2,NodesToGo,ELO):-
        E=edge(X,Y,R),
        member(Y,NodesToGo),
        !,
        findall(edge(X,Z,R2),member(edge(Y,Z,R2),EL),ELJ),
        findall(E2,(member(E2,EL),E2\=edge(Y,_,_)),ELRest),
                append(ELRest,ELJ,ELPush),

        % remove edge from final list
        debug(collapse,'rm: ~w',[E]),
        collapse_edgelist(ELJ,EL2,ELC,ELO).
*/

collapse_edgelist([E|EL],[E|EL2],ELC,ELO):-
        !,
        % pass through
        debug(collapse,'pass: ~w',[E]),
        collapse_edgelist(EL,EL2,ELC,ELO).
collapse_edgelist([],[],_,_):- !.

collapse_relation(R,Edge,EdgesIn):-
        Edge=edge(_,Y,R),
        member(edge(Y,_,R),EdgesIn).

user:collapse_is_a(Edge,EdgesIn):-
        Edge=edge(_,Y,relation_link(subclass)),
        member(edge(Y,_,relation_link(subclass)),EdgesIn).

relation_closure_predicate(R,inverse(P),Opts):-
        select(invert(1),Opts,OptsRemaining),
        !,
        relation_closure_predicate(R,P,OptsRemaining).
relation_closure_predicate(all,relation_link(_),_):- !.
relation_closure_predicate([],relation_link(_),_):- !. % empty=all
relation_closure_predicate([R],parent_over_nr(R,_),_):- !. % [R]->R
relation_closure_predicate(Rs,parent_over_oneof(_-Rs),_):- is_list(Rs),!.
relation_closure_predicate(R,parent_over_nr(R,_),_).

%% parent_over_oneof(+RTerm,?Node,?PNode)
% RTerm = Rel-Rels
% true if Rel is a member of Rels, and PNode can be reached from Node via Rel
parent_over_oneof(R-Rs,Node,PNode):- member(R,Rs),parent_over_nr(R,Node,PNode).

% inverts order of arguments so it can be used in a functional style
% (instances AND classes)
relation_link(R,Node,PNode):- node_link(Node,R,PNode),\+exclude_relation_in_closure(R).
%exclude_relation_in_closure(has_part).  % !!! HACK!! sometimes we want this. works fine for Cell in FMA
exclude_relation_in_closure(R):- is_cyclic(R).

% helper predicates (duped with blipkit_ontol)

% tree_node_ids(+N,?NodeSet) is semidet
% NodeSet has no dupes
% N=node(_,Node,SubNodes)
tree_node_ids(N,Nodes):-
        tree_node_id_list(N,Nodes1),
        list_to_set(Nodes1,Nodes).

% tree_node_ids(+N,?NodeList)t
% NodeList may have dupes
tree_node_id_list(node(_,Node,Nodes),[Node|Nodes]):-
        findall(SubNodes,(member(Node,Nodes),tree_node_id_list(Node,SubNodes)),SubNodesL),
        flatten(SubNodesL,Nodes).

%% subclass_lca(+IDs,?LCA) is semidet
subclass_lca(IDs,LCA):-
        setof(A,
              minimal_spanning_node(ontol_db:subclassRT,IDs,A),
              [LCA]).


%% class_pair_subclass_lca(+X,+Y,?LCA)
% true if LCA is a common ancestor of X and Y by subclassRT/2,
% and if there exists no other more recent CA
class_pair_subclass_lca(X,Y,LCA):-
        subclassRT(X,LCA),
        subclassRT(Y,LCA),
        \+ (( subclassRT(X,CA),
              subclassRT(Y,CA),
              subclassT(CA,LCA))).


% -------------------- STATS --------------------
:- multifile dbmeta:schema_statistic/2.

dbmeta:schema_statistic(ontol_db,count(classes,Num)):-
        setof_count(C,class(C),Num).
dbmeta:schema_statistic(ontol_db,count(instances,Num)):-
        setof_count(C,inst_of(C,_),Num).
dbmeta:schema_statistic(ontol_db,count(ontologies,Num)):-
        setof_count(O,belongs(_,O),Num).
dbmeta:schema_statistic(ontol_db,count_by(classes_by_ontology,Num)):-
        count_by(O,belongs(_C,O),Num).


% -------------------- TESTS --------------------
% regression tests to ensure behaviour of module is correct;
% lines below here are not required for module functionality

unittest(load(sofa)=
      load_biofile(obo,'sofa.obo')/[]).
unittest(load(fly)=
      load_bioresource(fly_anatomy)/[]).

unittest(load(parent_over)=
      load_biofile(go,'parent_over_test.go')/[]).

unittest(test(fly1,
            [_=load(fly)],
            (   ensure_loaded(bio(ontol_db)),
                ensure_loaded(bio(graph)),
                ensure_loaded(bio(bioprolog_util)),
                class(C,'head sensillum'),
                solutions(P,closure(ontol_db:parent_over_nr(part_of),C,P),Ps),
                writeln(parents=Ps),
                nl
                ),
            member('FBbt:00000001',Ps))).
unittest(test(fly2,
            [_=load(fly)],
            (   ensure_loaded(bio(ontol_db)),
                ensure_loaded(bio(graph)),
                ensure_loaded(bio(bioprolog_util)),
                class(TestC,'head sensillum'),
                closure_to_edgelist_delta(ontol_db:parent_over_nr(part_of),'FBbt:00000001',Edges,down(2)),
                writeln(edges=Edges),
                solutions(C,member(edge(C,_,_),Edges),Cs),
                writeln(cs=Cs),
                nl
                ),
            member(TestC,Cs))).

unittest(test(sofa,
            [_=load(sofa)],
            (   ensure_loaded(bio(ontol_db)),
                findall(PID,subclassT('SO:0000234',PID),PIDs),
                writeln(pidsA=PIDs),
                nl
                ),
            member('SO:0000673',PIDs))).

unittest(test(parent_over,
            [_=load(parent_over)],
            (   ensure_loaded(bio(ontol_db)),
                parentT('Y:4','X:1'),
                findall(SuperPart,parent_over(part_of,'Y:4',SuperPart),SuperParts),
                writeln(superparts=SuperParts),
                parent_over(part_of,'Y:3','X:3'),
                parent_over(part_of,'Y:4','X:1'),
                findall(SuperPart,parent_over_nr(part_of,'Y:4',SuperPart),SuperPartsNR),
                writeln(superpartsNR=SuperPartsNR),
                findall(SuperPart,parent_over_nr(part_of,'Y:2',SuperPart),Y2SuperPartsNR),
                writeln(y2superpartsNR=Y2SuperPartsNR),
                !,
                parent_over_nr(part_of,'Y:2','X:1'), %% ?
                nl
                ),
            SuperPartsNR=['X:3'])).

unittest(test(parent_over_with_list,
            [_=load(parent_over)],
            (   ensure_loaded(bio(ontol_db)),
                findall(SuperPart,parent_over([part_of,subclass],'Y:4',SuperPart),SuperParts),
                writeln(superparts=SuperParts),
                nl
            ),
            true)).

/** <module> ontology classes and instances

  ---+ Synopsis
  
  ==
  :- use_module(bio(ontol_db)).
  :- use_module(bio(io)).

  % find all superclasses of a particular class
  demo:-
    load_bioresource(go),
    class(ID,'transcription factor activity'),
    setof(Parent,subclassRT(ID,Parent),Parents),
    writeln(parents=Parents).    
  ==

  
  ---+ Package

  This module is part of the blipkit ontol package. See
  README.txt
  
  ---+ Description

  This module contains defines extensional and intensional data
predicates that model ontology and instance data. The data model is
obo-like, and can easily be mapped to an OWL ontology (see bridge)

  The basic querying is such things as transitive relationships and
  detecting illegal cycles or redundant relationships. It requires
  ontology prolog fact files containing at least the following facts:

  
    * class/2
    * belongs/2
    * subclass/2
    * restriction/3
  
  
  Any cycles over subclass/2 or restriction/3 will cause
  non-terminating behavior

  ---++ Importing and exporting data

  This is a data module. Facts can be imported and exported from both
prolog fact databases and other formats
  
  ---+++ Import

  The following file formats can be read in using load_biofile/2
and load_bioresource/1

  
  * obo
  * obo_xml
  * go
  * owl
  

  (See below for full details)
  
  The following database schemas have adapters:

  
  * gosql
  
  
  ---+++ Export

  The following file formats can be written using write_biofile/2

  
  * OWL - (ensure_loaded ontol_bridge_to_owl)
  

  ---+++ Obo-format files

  This is a format commonly used for bio-ontologies
  See <http://www.geneontology.org> GO

  Blip contains an experimental pure-prolog parser (see
parser_obo), which can be invoked by using the file format atom
'obo_native'. For now the default is to invoke the go-perl go2prolog
script which generates prolog fact files. This should all happen
behind the scenes, just specifify the file format 'obo'
  
  ---+++ The ontol module and RDF/OWL

  The data predicates defined in this module map fairly well to
OWL. As well as loading the data predicates directly from an
OBO-formatted file (as is common in the bio-ontologies world), the
data predicates can map to predicates defined in the SWI-Prolog rdb_db
module - see
<ontol_bridge_from_owl.html> ontol_bridge_from_owl

  ---+++ Instance data

  As well as modeling ontologies consisting purely of class data, this
module also handles instance data. A generic free model is used to
store instances, similar to most frame-based systems, RDF and OWL. In
principle, any kind of data can be modeled as instances belong to a
particular class. This means that the ontol module can provide an
object system for modeling data. See inst/2, inst_rel/3 and
inst_sv/4 for details

  ---++ Predicate naming conventions

  Typically the modes of the inference predicates are defined such
  that parents are infered from children, rather than vice versa

  
    * T - transitive
    * R - reflexive (includes self)
    * RT - reflexive transitive
    * N - uses names rather than IDs to refer to classes

  ---+ Additional Information

  This module is part of blip. For more details, see http://www.blipkit.org
  

*/
