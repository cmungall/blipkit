/* -*- Mode: Prolog -*- */

:- module(ontol_bridge_to_owl2,[
                               ]).
:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).
:- use_module(bio(rdf_id_util),[rdfid_oboid/2]).
:- use_module(library('thea2/owl2_model'),
	      []).

native_to_literal(Term,literal(Term)):- number(Term),!.
native_to_literal(Term,literal(lang(en,Term))).

% ----------------------------------------
% METADATA
% ----------------------------------------

% only convert a minimal amount - leave the rest to bridge modules

owl2_model:propertyAssertion('http://www.w3.org/2000/01/rdf-schema#label',UA,UX) :-
        rdfid_oboid(UA,A),
        entity_label(A,X),
        native_to_literal(X,UX).

% ----------------------------------------
% DECLARATIONS
% ----------------------------------------

owl2_model:class(UA) :- rdfid_oboid(UA,A),class(A).
owl2_model:objectProperty(UA) :- rdfid_oboid(UA,A),property(A),\+is_class_level(A).
owl2_model:individual(UA) :- rdfid_oboid(UA,A),inst(A).

% ----------------------------------------
% PROPERTIES
% ----------------------------------------

owl2_model:transitiveProperty(UA) :- rdfid_oboid(UA,A),is_transitive(A),\+is_class_level(A).
owl2_model:symmetricProperty(UA) :- rdfid_oboid(UA,A),is_symmetric(A),\+is_class_level(A).

owl2_model:inverseProperties(UA,AB) :- rdfid_oboid(UA,A),rdfid_oboid(UB,B),inverse_of(A,B).
owl2_model:subPropertyOf(propertyChain([UA,UB]),UA) :- rdfid_oboid(UA,A),rdfid_oboid(UB,B),transitive_over(A,B).
owl2_model:subPropertyOf(propertyChain([UA,UB]),UC) :- rdfid_oboid(UA,A),rdfid_oboid(UB,B),rdfid_oboid(UC,C),holds_over_chain(C,[A,B]).
owl2_model:subPropertyOf(propertyChain([UA,UB]),UC) :- rdfid_oboid(UA,A),rdfid_oboid(UB,B),rdfid_oboid(UC,C),equivalent_to_chain(C,[A,B]).
                        

% ----------------------------------------
% AXIOMS
% ----------------------------------------

owl2_model:subClassOf(UA,UB) :-
	rdfid_oboid(UA,A),rdfid_oboid(UB,B),subclass(A,B).
owl2_model:subClassOf(UA,Expr):-
	rdfid_oboid(UA,A),
	restriction(A,P,B),
	pval_expr(P,B,Expr).

owl2_model:equivalentClasses([UA,UB]) :-
	rdfid_oboid(UA,A),
	rdfid_oboid(UB,B),
        equivalent_to(A,B).

owl2_model:equivalentClasses([UA,intersectionOf([UG|Restrs])]) :-
	rdfid_oboid(UA,A),
	rdfid_oboid(UG,G),
        genus(A,G),
        findall(Restr,(differentium(A,P,B)),pval_expr(P,B,Restr)),Restrs).

% ----------------------------------------
% CLASS EXPRESSIONS
% ----------------------------------------

% the default is an existential restriction. obo also allows
% cardinality, but care must be taken with transitive properties
% (OWL-DL does not allow QCRs on transitive properties).

pval_expr(card(P,_,0 ),B,allValuesFrom(UP,complementOf(UB))) :- 
	rdfid_oboid(UB,B),
	rdfid_oboid(UP,P),
        !.
pval_expr(card(P,N),B,minCardinality(N,UP,UB)) :- 
	rdfid_oboid(UB,B),
	rdfid_oboid(UP,P),
        \+ is_transitive(P),
        !.
pval_expr(card(P,N,N),B,exactCardinality(N,UP,UB)) :- 
	rdfid_oboid(UB,B),
	rdfid_oboid(UP,P),
        \+ is_transitive(P),
        !.
pval_expr(card(P,Min,Max),B,intersectionOf([minCardinality(Min,UP,UB),maxCardinality(Max,UP,UB)])) :- 
	rdfid_oboid(UB,B),
	rdfid_oboid(UP,P),
        \+ is_transitive(P),
        !.
pval_expr(card(P,_),B,someValuesFrom(UP,UB)) :- % no cardinality on transitive in OWL-DL
	rdfid_oboid(UB,B),
	rdfid_oboid(UP,P),
        is_transitive(P),
        !.
pval_expr(card(P,_,_),B,someValuesFrom(UP,UB)) :- % no cardinality on transitive in OWL-DL
	rdfid_oboid(UB,B),
	rdfid_oboid(UP,P),
        is_transitive(P),
        !.
pval_expr(P,B,someValuesFrom(UP,UB)) :- % GUESS existential
	rdfid_oboid(UB,B),
	rdfid_oboid(UP,P),
        \+ is_class_level(P),
        !.
pval_expr(P,B,hasValue(UP,UB)) :- % GUESS existential
	rdfid_oboid(UB,B),
	rdfid_oboid(UP,P),
        is_class_level(P),
        !.
pval_expr(R,To,_) :- throw(pval_expr('cannot translate ~w ~w',[R,To])).
