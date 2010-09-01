/* -*- Mode: Prolog -*- */
:- module(ontol_db,
          [
           ontology/1,
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
           is_proper/1,
           expand_expression_to/2,
           expand_assertion_to/2,
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
           inst_rel/3,
           inst_rel/4,
           inst_rel/5,
           inst_rel/6,
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
	   
           % intensional predicates
           class/2,
           property/2,
           inst/2,
           referenced_id/2,
           class_label/3,
           class_label_exact/2,
           topclass/1,
           topclass/2,
           noparent/1,
           noparent/2,
           nochild/1,
           nochild/2,
           subclassT/2,
           subclassRT/2,
           subclassX/2,
           subclassXT/2,
	   cdef_placement/5,
           node_link/3,
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
	   idspace_references/2,
	   idspace_references_reflexive/2,
           idspace_mireot/2,
           idspace_mireot/3,
	   bf_parentRT/2,
	   bf_set_parentRT/2,
           combine_relation_pair/3,
           inferred_parent/2,           
           inferred_parent_via/3,
           inferred_parent_dist/3,
           inferred_parent_dist_via/4,
	   strict_subclass/2,
	   cdef_label/2,
           class_cdef/2,
           
           subclass_cycle/2,
           parent_cycle/2,

           redundant_subclass/3,
           redundant_parent/5,
           subclass_underlap/3,

           subclass_lca/2,
           class_pair_subclass_lca/3,
           
           inst_ofRT/2
          ]).

% metadata on the data predicates used in this module
:- use_module(bio(dbmeta)).
:- use_module(bio(metadata_db)).
:- use_module(bio(bioprolog_util)).  % required for statistics
:- use_module(bio(macros_transitive)).

:- use_module(library(ordsets)).

:- pure
           class/2,
           property/2,
           slot/2,
           inst/2,
           referenced_id/2,
           class_label/3,
           class_label_exact/2,
           topclass/1,
           topclass/2,
           noparent/1,
           noparent/2,
           nochild/1,
           nochild/2,
           subclassT/2,
           subclassRT/2,
           child/2,
           child/3,
           node_link/3,
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
           
           redundant_subclass/3,
           redundant_parent/5,
           subclass_underlap/3,
           multiple_parent/4,
           multiple_parent/5,

           subclass_lca/2,
           class_pair_subclass_lca/3,
           
           % experimental

           inst_ofRT/2.


% ----------------------------------------
% ONTOLOGIES            
% ----------------------------------------

%%  ontology(?Ontology) is nondet.
% e.g. ontology('GO')
:- extensional(ontology/1).

%%  ontology(?Ontology,?Name,?Desc) is nondet.
% DEPRECATED
:- extensional(ontology/3).

%% ontology(?Ontology,?Name)
% DEPRECATED
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

% ----------------------------------------
% CLASSES
% ----------------------------------------

%%  class(?Class) is nondet.
% class declaration - true if Class is a class
:- extensional(class/1).

%%  class(?Class,?Label) is nondet.
%  combines class/1 and entity_label/2
:- multifile class/2.
class(C,Name):- entity_label(C,Name),class(C).

class_id_or_label(ID,N):- (class(ID,N)-> true ; N=ID).


%%  subclass(?Class,?SuperClass) is nondet.
%    An asserted is_a (subtype, subsumption) relationship
%    equivalent to owl:subClassOf
%    transitive form is subclassT/2
:- extensional(subclass/2).



%%  equivalent_class(?Class1,?Class2) is nondet.
%    corresponds to owl:equivalentClass
:- extensional(equivalent_class/2).

/*
%% class_equivalence_set(?Class,?ClassSet)
% true if Class is in ClassSet
class_equivalence_set(C,S) :-
        equivalent_class(C,_),
	expand_equivset([C],[],[],S).
	
expand_equivset([ID|IDs],DoneIDs,Ancs,AncsFinal) :-
	setof(XID,(equivalent_class(ID,XID),
                   \+member(XID,IDs),
                   \+member(XID,DoneIDs)),Parents),
	!,
	ord_union(Parents,IDs,NextIDs),
	ord_union(Ancs,Parents,AncsNew),
	expand_equivset(NextIDs,[ID|DoneIDs],AncsNew,AncsFinal).
expand_equivset([ID|IDs],DoneIDs,Ancs,AncsFinal) :-
	!,
	expand_equivset(IDs,[ID|DoneIDs],Ancs,AncsFinal).
expand_equivset([],_,Ancs,Ancs).
*/



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

        


%% node_link(Node,Relation,Link) is nondet.
% true if parent/3 or inst_rel/3
% TODO: check semantics
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


%% parentT(?Class,?ParentClass) is nondet.
% see parentT/3
parentT(ID,IDp):- parentT(ID,_,IDp).

%% parentT(?Class,?Relation,?ParentClass) is nondet.
% transitive parent/3
% new: now uses inferred_parent_via/3
parentT(ID,R,PID):-
        inferred_parent_via_rev(ID,PID,[R]).
	%parent_overT(R,ID,PID).

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
parentRT(ID,R,ID) :- is_reflexive(R).
parentRT(ID,TL,IDp):- parentT(ID,TL,IDp).



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


% ----------------------------------------
% RELATIONS
% ----------------------------------------

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
% EXPERIMENTAL
:- extensional(complement_of/2).

% DEPRECATED
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
% TODO: DEPRECATE? obof1.3, no longer valid in obof1.4
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


%%  is_proper(?Relation) is nondet.
:- extensional(is_proper/1).

%% expand_expression_to(?Rel,?ExprAtom)
% OWL macros
:- extensional(expand_expression_to/2).

%% expand_assertion_to(?Rel,?ExprAtom)
% OWL macros
:- extensional(expand_assertion_to/2).



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

% ----------------------------------------
% INSTANCES
% ----------------------------------------

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

% DEPRECATED - all relations are now binary
:- extensional(inst_rel/4).
:- extensional(inst_rel/5).
:- extensional(inst_rel/6).
:- extensional(inst_rel/7).

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

% ----------------------------------------
% OTHER
% ----------------------------------------

%% class_instrule(?Class,?HeadVars,?PrologRuleBody)
% Class(HeadVars) :- Body is a rule for instantiating Class
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
% Expression may be 'true' if deduction provenance is not maintained.
%
% see ontol_reasoner.pro
:- extensional(entailed_by/2).

%% entailed_by(?Statement,?Rule,?Expression) is nondet.
:- extensional(entailed_by/3).

%% logicalformula(?ID,?FormulaAtom,?Language) is nondet.
% DEPRECATED
:- extensional(logicalformula/3).

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

%% referenced_id(?ID,?RefID)
%
%  ID references RefID if RefID appears as object of
%subclass/restriction/intersection relation
referenced_id(ID,RefID):-     restriction(ID,_,RefID).
referenced_id(ID,RefID):-     subclass(ID,RefID).
referenced_id(ID,RefID):-     differentium(ID,_,RefID).
referenced_id(ID,RefID):-     genus(ID,RefID).

% ----------------------------------------
% INTER-ONTOLOGY REFERENCES AND MIREOT
% ----------------------------------------

%% idspace_references(?S,?Ref)
% true if Ref is in the bf_parentRT/2 closure of a class
% in idspace S.
%
% this predicate can be used for MIREOTing
idspace_references(S,Ref) :-
	setof(X,(class(X),
		 id_idspace(X,S)),
	      Xs),
	member(X,Xs),
	bf_parentRT(X,Ref),
	\+ id_idspace(Ref,S).

%% idspace_references_reflexive(?S,?Ref)
% as idspace_references/2, but includes all classes in S
idspace_references_reflexive(S,Ref) :-
	idspace_references(S,Ref).
idspace_references_reflexive(S,Ref) :-
	class(Ref),
	id_idspace(Ref,S).

%% idspace_mireot(+IDSpace,?RefEntity)
% true if RefEntity is referenced directly or indirectly from an entity in IDSpace
idspace_mireot(SX,Ref) :-
        idspace_mireot(SX,Ref,_).

%% idspace_mireot(+IDSpace,?RefEntity,+RefEntityIDSpace)
%
% idspace_mireot/2 with the additional constraint that directly referenced entities must come from RefEntityIDSpace.
% if RefEntityIDSpace is var, then the constraint is that the directly referenced entity does not come from IDSpace.
% TODO: change this?
%
idspace_mireot(SX,Ref,SRef) :-
	solutions(X,(class(X),
                     id_idspace(X,SX),
                     parent(X,Y),
                     id_idspace(Y,SRef),
                     SRef\=SX),
                  Xs),
	member(X,Xs),
        bf_parentRT(X,Ref), % no need to table
        id_idspace(Ref,SRef),
        SRef\=SX.


% ----------------------------------------
% SIMPLE GRAPH CLOSURE
% ----------------------------------------
% uses breadth-first traversal, ignores relationship type

%% bf_parentRT(?ID,?PID)
% true if PID is reachable from ID via parent/2.
%
% use breadth-first traversal
% cycle-safe
bf_parentRT(ID,PID) :-
	class(ID),
	debug(bf_parentRT,'bf_parentRT(~w)',[ID]),
	ids_ancestors([ID],[],[],L),
	member(PID,L).
bf_parentRT(ID,ID) :-
	class(ID).

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

%% bf_set_parentRT(IDs:list,PID)
% as bf_parentRT/2, starting point is a list
bf_set_parentRT(IDs,PID) :-
	ids_ancestors(IDs,[],[],PIDs),
	member(PID,PIDs).
bf_set_parentRT(IDs,PID) :-
	member(PID,IDs). % TODO - unique results only

% ----------------------------------------
% GRAPH INFERENCE
% ----------------------------------------
% relational path from a term to its ancestor

%% inferred_parent(+EntityID,?InferredParentID) is nondet
inferred_parent(ID,PID) :-
        inferred_parent_via_rev(ID,PID,_).

%% inferred_parent(+EntityID,?InferredParentID,?Over) is nondet
inferred_parent_via(ID,PID,Over) :-
        inferred_parent_via_rev(ID,PID,OverRev),
        reverse(OverRev,Over).

inferred_parent_via_rev(ID,PID,Over) :-
        (   var(ID)
        ->  class(ID)
        ;   true),
	entity_relations_closure([ID-[]],[],[],L),
	member(PID-Over,L).

%% entity_relations_closure(+ScheduledCCPairs,+Visited,+AccumulatedResults,?FinalResults)
entity_relations_closure([Class-Conns|ScheduledCCPairs],Visited,ResultCCPairs,FinalCCPairs) :-
	setof(Parent-NewConns,
              (   entity_parent_chain(Class,Parent,Conns,NewConns),
                  \+ord_memberchk(Parent-NewConns,Visited)),
              NextCCPairs),
	!,
	ord_union(ResultCCPairs,NextCCPairs,ResultCCPairsNew),
        ord_union(ScheduledCCPairs,NextCCPairs,NewScheduledCCPairs),
	entity_relations_closure(NewScheduledCCPairs,[Class-Conns|Visited],ResultCCPairsNew,FinalCCPairs).
entity_relations_closure([Class-Conns|ScheduledCCPairs],Visited,ResultCCPairs,FinalCCPairs) :-
	!,
        % Class has no parents
	entity_relations_closure(ScheduledCCPairs,[Class-Conns|Visited],ResultCCPairs,FinalCCPairs).
entity_relations_closure([],_,ResultCCPairs,ResultCCPairs).

entity_parent_chain(Class,Parent,InConns,NewConns) :-
        parent(Class,ConnNext,Parent),
        combine_relation_pairs(InConns,ConnNext,NewConns).

% note that connection list maintained in reverse order
combine_relation_pairs([ConnPrev|InConns],ConnNext,NewConns) :-
        combine_relation_pair(ConnPrev,ConnNext,NewConn),
        !,
        combine_relation_pairs(InConns,NewConn,NewConns).
combine_relation_pairs(InConns,ConnNext,[ConnNext|InConns]). % top of stack

% composition table A B -> C
combine_relation_pair(subclass,subclass,subclass).
combine_relation_pair(subclass,R,R) :- all_some(R).
combine_relation_pair(R,subclass,R) :- all_some(R).
combine_relation_pair(R,R,R) :- is_transitive(R).
combine_relation_pair(R1,R2,R) :- holds_over_chain(R,[R1,R2]).
combine_relation_pair(R1,R2,R) :- equivalent_to_chain(R,[R1,R2]).

% same with distances

inferred_parent_dist(ID,PID,Dist) :-
        inferred_parent_dist_via_rev(ID,PID,Dist,_).

%% inferred_parent_dist(+EntityID,?InferredParentID,?Dist,?Over) is nondet
inferred_parent_dist_via(ID,PID,Dist,Over) :-
        inferred_parent_dist_via_rev(ID,PID,Dist,OverRev),
        reverse(OverRev,Over).

inferred_parent_dist_via_rev(ID,PID,Dist,Over) :-
        (   var(ID)
        ->  class(ID)
        ;   true),
	entity_reldists_closure([ID-[]-0],[],[],L),
	member(PID-Over-Dist,L).

%% entity_reldists_closure(+ScheduledCCPairs,+Visited,+AccumulatedResults,?FinalResults)
entity_reldists_closure([Class-Conns-CurDist|ScheduledCCPairs],Visited,ResultCCPairs,FinalCCPairs) :-
        NextDist is CurDist+1,
	setof(Parent-NewConns-NextDist,
              (   entity_parent_chain(Class,Parent,Conns,NewConns),
                  \+ord_memberchk(Parent-NewConns-NextDist,Visited)),
              NextCCPairs),
	!,
	ord_union(ResultCCPairs,NextCCPairs,ResultCCPairsNew),
        ord_union(ScheduledCCPairs,NextCCPairs,NewScheduledCCPairs),
	entity_reldists_closure(NewScheduledCCPairs,[Class-Conns-NextDist|Visited],ResultCCPairsNew,FinalCCPairs).
entity_reldists_closure([CCD|ScheduledCCPairs],Visited,ResultCCPairs,FinalCCPairs) :-
	!,
        % Class has no parents
	entity_reldists_closure(ScheduledCCPairs,[CCD|Visited],ResultCCPairs,FinalCCPairs).
entity_reldists_closure([],_,ResultCCPairs,ResultCCPairs).



% -----------------------------------
% BACKWARD CHAINING REASONING
% -----------------------------------
% non-cycle safe


%%  subclassT(?Class,?SuperClass) is nondet.
% transitive form of subclass/2
subclassT(X,Y):- subclass(X,Y).
subclassT(X,Y):- subclass(X,Z),subclassT(Z,Y).

%%  subclassRT(?Class,?SuperClass) is nondet
% reflexive form of subclassT/2
subclassRT(X,X).
subclassRT(X,Y):- subclassT(X,Y).


% -----------------------------------
% FAST REASONING OVER DEFINITIONS
% -----------------------------------

%% subclassXT(?A,?B)
% reflexive version of subclassX/2
subclassXT(A,B) :-
	subclassXT(A,B,[]).

subclassXT(A,A,_).
subclassXT(A,B,VL) :-
	subclassX(A,B,VL).
	

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
	subclassX(A,B,[]).

subclassX(A,B,VL) :-
        %debug(subclassX,'testing subclass(~w,~w) visited: ~w',[A,B,VL]),
	(   class_cdef(A,A1)
	;   A1=A),
	(   class_cdef(B,B1)
	;   B1=B),
	subclassX_2(A1,B1,VL).
%subclassX(A,B,asserted) :-
%	subclassT(A,B).

	
% N+S conditions, any < intersection
subclassX_2(A,cdef(BG,BDs),VL) :-
	subclassXT(A,BG,VL),
	forall(member(BD,BDs),
	       subclassXT(A,BD,VL)).
% N+S conditions, class < rel-expr
subclassX_2(A,BR=BV,VL) :-
	class(A),
	parent(A,AR,AV),
	\+ member(AV,VL),
	subclassX_2(AR=AV,BR=BV,[AV|VL]).
        %% parent_overT(R,A,V). -- TODO
% N+S conditions, rel-expr < rel-expr
subclassX_2(AR=AV,BR=BV,_) :-
	subclassRT(AR,BR),
	subclassRT(AV,BV).
% AR=AV < BR=BV if (AR=AV < R=X < BR=BV)
% e.g.
% part_of(A) < part_of(C) if A<part_of(C)
subclassX_2(R=X,TR=Y,VL) :-
	is_transitive(TR),
	subclassRT(R,TR),
	subclassRT(X,X1),
	\+ member(X1,VL),
	subclassX_2(X1,TR=Y,[X1|VL]).
% N+S conditions, intersection < rel-expr [genus test]
subclassX_2(cdef(AG,_),R=V,VL) :-
	subclassX_2(AG,R=V,VL).
% N+S conditions, intersection < rel-expr [differentiae test]
subclassX_2(cdef(_,ADs),BR=BV,VL) :-
	member(AR=AV,ADs),
	subclassX_2(AR=AV,BR=BV,VL).
% N+S conditions, intersection < class
subclassX_2(cdef(AG,_),B,_) :-
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

%% cdef_placement(+CDef,?EquivClasses,?NRParents,?NRChildren,?RedundantSubClassPairs)
cdef_placement(CDef,Equiv,NRParents,NRChildren,Redundant) :-
        debug(cdef_placement,'finding parents of ~w',[CDef]),
	solutions(Parent,(class_cdef(Parent,ParentCDef),
                          subclassX(CDef,ParentCDef)),InferredParents),
        (   InferredParents=[]
        ->  CDef=cdef(Parent,_),
            Parents=[Parent]    % default to genus
        ;   Parents=InferredParents),
        debug(cdef_placement,'finding equivs of ~w',[CDef]),
	solutions(Parent,(member(Parent,Parents),subclassX(Parent,CDef)),Equiv),
        debug(cdef_placement,'finding children of ~w',[CDef]),
	solutions(Child,(class(Child),
                         subclassX(Child,CDef)),Children),
        debug(cdef_placement,'   children = ~w',[Children]),
        debug(cdef_placement,'finding redundant subclass/2 facts introduced by ~w',[CDef]),
	solutions(Child-Parent,(member(Parent,Parents),subclass(Child,Parent),member(Child,Children)),Redundant),
	solutions(Parent,(member(Parent,Parents),\+((subclass(P2,Parent),member(P2,Parents)))),NRParents),
	solutions(Child,(member(Child,Children),\+((subclass(Child,C2),member(C2,Children)))),NRChildren).


%% cdef_label(+CDef,?Label)
% generates a label from a cdef
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

% ----------------------------------------
% UTIL
% ----------------------------------------

%% strict_subclass(?A,?B)
% true if A is a subclass of B, and there is no Z
% such that a is a subclass of Z.
% this is always true of subclass/2 in a single-inheritance hierarchy
strict_subclass(A,B) :-
	subclass(A,B),
	\+((subclass(A,Z),Z\=B)).

% ----------------------------------------
% DEPRECATED
% ----------------------------------------

%% parent_over(+R,?Class,?ParentClass) is nondet.
% as parent_over/4
% DEPRECATED
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
% DEPRECATED - this is now the same as parentT/3 and
% inferred_parent_via/3 with chains of length=1
parent_overT(R,ID,PID) :- inferred_parent_via_rev(ID,PID,[R]).

%% parent_overT(+R,?Via,?Class,?ParentClass) is nondet.
% DEPRECATED - this is now the same as inferred_parent_via/3
parent_overT(R,ID,PID,[R|Path]) :-
        inferred_parent_via(ID,PID,[R|Path]).



/*
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
*/

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
% DEPRECATED
% now operates as parent/3
parent_over_nr(R,direct,ID,PID):-
        parent(ID,R,PID).

/*
% experiment with deprecating this:
parent_over_nr(R,Via,ID,PID):-
        parent_over(R,Via,ID,PID),
        \+ (   parent_over(R,ID,IID),
               (   parent_over(R,IID,PID)
               ;   subclassT(IID,PID))),
        \+ (subclassT(ID,IID),parent_over(R,IID,PID)).
*/

% ----------------------------------------
% DISJOINT REASONING
% ----------------------------------------

%%  class_disjoint_union_list(?Class,?JEPDClassList) is nondet.
% inferred from disjoint_from/2 and class_union_element/2
class_disjoint_union_list(Class,Us):-
        setof(U,class_union_element(Class,U),Us),
        forall(member(C1,Us),
               forall((member(C2,Us),C1\=C2),
                      disjoint_from(C1,C2))).

%% disjoint_from_violation(?P1,?P2,?C)
% true if disjoint_from/2 holds between P1 and P2,
% and C is a subclass/2 of both
disjoint_from_violation(P1,P2,C):-
        disjoint_from(P1,P2),
        subclassT(C,P1),
        subclassT(C,P2).

%% disjoint_over_violation(?DR,?X,?Y,?A)
% true if X is declared to be disjoint with Y over some relation R,
% and there is some A that stands in relation R to X and Y
%
% this is slow - recommended use ontol_reasoner instead
disjoint_over_violation(DR,X,Y,A):-
        disjoint_over(DR,R),	% e.g. disjoint_over(disconnected_from,part_of)
        (   restriction(X,DR,Y)
        ;   restriction(Y,DR,X)),
        debug(ontol,'testing disjoint_over ~w :: ~w ~w ~w',[R,X,DR,Y]),
        parent_overRT(R,A,X),
        parent_overRT(R,A,Y).



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


%% noparent(?Class,?Ontology)
% true if Class is top of union of subclass and restriction hierarchy within Ontology
noparent(C,O):-
        belongs(C,O),
        \+((parent(C,PID),belongs(PID,O))).

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


%% subclass_underlap(?C,?P1,?P2) is nondet.
% C is_a P1 and C is_a P2
% P1 and P2 are non-disjoint because o C
subclass_underlap(C,P1,P2):-
        subclassT(C,P1),
        subclassT(C,P2),
        P1\=P2.

% ----------------------------------------
% CYCLE CHECKS
% ----------------------------------------

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


% -----------------------------------
% LCA
% -----------------------------------

%% subclass_lca(+IDs:list,?LCA) is semidet
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

:- multifile dbmeta:fact_chain_hook/2.
dbmeta:fact_chain_hook(class(C),
	   [subclass(C,_),
	    restriction(C,_,_),
	    genus(C,_),
	    differentium(C,_,_),
	    metadata_db:entity_label(C,_),
	    metadata_db:entity_resource(C,_),
	    metadata_db:entity_partition(C,_)
	    ]).

% -----------------------------------
% STATS GENERATION
% -----------------------------------

:- multifile dbmeta:schema_statistic/2.

dbmeta:schema_statistic(ontol_db,count(classes,Num)):-
        setof_count(C,class(C),Num).
dbmeta:schema_statistic(ontol_db,count(instances,Num)):-
        setof_count(C,inst_of(C,_),Num).
dbmeta:schema_statistic(ontol_db,count(ontologies,Num)):-
        setof_count(O,belongs(_,O),Num).
dbmeta:schema_statistic(ontol_db,count_by(classes_by_ontology,Num)):-
        count_by(O,belongs(_C,O),Num).


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

  ---+ See Also

  Thea

*/
