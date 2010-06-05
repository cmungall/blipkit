/* -*- Mode: Prolog -*- */


:- module(ontol_bridge_to_obolog,[
                                  ]).
:- use_module(bio(ontol_db),[]).
:- use_module(bio(metadata_db),[]).
:- use_module(bio(obolog_db),[]).

% currently slow because all forumlae are reified
% todo: realize/materialize

obolog_db:formula(type(A)):- ontol_db:class(A).
obolog_db:formula(relation(A)):- ontol_db:property(A).
obolog_db:formula(instance(A)):- ontol_db:inst(A).
obolog_db:formula(is_a(A,B)):- ontol_db:subclass(A,B),\+ \+ (( ontol_db:class(A) ; ontol_db:class(B))).
obolog_db:formula(subrelation(A,B)):- ontol_db:subclass(A,B),\+ (( ontol_db:class(A) ; ontol_db:class(B))).
obolog_db:formula(instance_of(A,B)):- ontol_db:inst_of(A,B).
obolog_db:formula(disjoint_from(A,B)):- ontol_db:disjoint_from(A,B),ontol_db:class(A),ontol_db:class(B).
obolog_db:formula(relation_disjoint_from(A,B)):- ontol_db:disjoint_from(A,B),ontol_db:property(A),ontol_db:property(B).
obolog_db:formula(disjoint_over(A,B)):- ontol_db:disjoint_over(A,B).
obolog_db:formula(relation_complement_of(A,B)):- ontol_db:complement_of(A,B),ontol_db:property(A).
obolog_db:formula(domain(A,B)):- ontol_db:property_domain(A,B).
obolog_db:formula(range(A,B)):- ontol_db:property_range(A,B).
obolog_db:formula(inverse_of(A,B)):- ontol_db:inverse_of(A,B).
obolog_db:formula(transitive_over(A,B)):- ontol_db:transitive_over(A,B).
obolog_db:formula(holds_over_chain(A,B,C)):- ontol_db:holds_over_chain(A,[B,C]).
obolog_db:formula(holds_over_chain(A,B,C,D)):- ontol_db:holds_over_chain(A,[B,C,D]). % TODO - etc
obolog_db:formula(equivalent_to_chain(A,B,C)):- ontol_db:equivalent_to_chain(A,[B,C]).
obolog_db:formula(equivalent_to_chain(A,B,C,D)):- ontol_db:equivalent_to_chain(A,[B,C,D]). % TODO - etc
obolog_db:formula(equivalent_to(R,I)):- ontol_db:property_intersection_elements(R,L),I=..[intersection_of|L].
obolog_db:formula(equivalent_to(R,I)):- ontol_db:property_union_elements(R,L),I=..[union_of|L].

% this is an odd hack to make up for the fact that most obo files are underspecified. TODO: don't write this if all-some specified already
%obolog_db:formula(all_some(A,B)):- ontol_db:property(A), \+ ontol_db:is_metadata_tag(A), atom_concat(A,'__instance_level',B).
obolog_db:formula(S):- ontol_db:property_relationship(R1,PR,R2),S=..[PR,R1,R2].



obolog_db:formula(T):- ontol_db:restriction(A,R,B),T=..[R,A,B].
obolog_db:formula(T):- ontol_db:inst_rel(A,R,B),T=..[R,A,B].
obolog_db:formula(T):- ontol_db:inst_sv(A,R,B),T=..[R,A,B]. % todo: datatypes

obolog_db:formula(functional(A)):- ontol_db:is_functional(A).
obolog_db:formula(reflexive(A)):- ontol_db:is_reflexive(A).
obolog_db:formula(transitive(A)):- ontol_db:is_transitive(A).
obolog_db:formula(symmetric(A)):- ontol_db:is_symmetric(A).
obolog_db:formula(anti_symmetric(A)):- ontol_db:is_anti_symmetric(A).

obolog_db:formula(label(A,B)):- metadata_db:entity_label(A,B).
obolog_db:formula(exact_synonym(A,B)):- metadata_db:entity_synonym_scope(A,B,exact).
obolog_db:formula(related_synonym(A,B)):- metadata_db:entity_synonym_scope(A,B,related).
obolog_db:formula(narrow_synonym(A,B)):- metadata_db:entity_synonym_scope(A,B,narrow).
obolog_db:formula(broad_synonym(A,B)):- metadata_db:entity_synonym_scope(A,B,broad).

obolog_db:formula(text_definition(A,B)):- ontol_db:def(A,B).
obolog_db:formula(text_definition_xref(A,B)):- ontol_db:def_xref(A,B).

obolog_db:formula(genus(A,B)):- ontol_db:genus(A,B).
obolog_db:formula(differentium(A,R,B)):- ontol_db:differentium(A,R,B).


/** <module> maps between ontol_db model and obolog_db

  ---+ Synopsis

==
:- use_module(bio(ontol_bridge_to_obolog)).

% 
demo:-
  nl.
  

==

---+ Details



@author  Chris Mungall
@version $Revision$
@see     README
@license License


*/
