/* -*- Mode: Prolog -*- */

:- module(ontol_bridge_to_owl2,[
                               ]).
:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).
:- use_module(bio(rdf_id_util),[rdfid_oboid/2]).
:- use_module(library('thea/owl2_model'),
	      []).

native_to_literal(Term,literal(Term)):- number(Term),!.
native_to_literal(Term,literal(lang(en,Term))).

owl2_model:class(UA) :- rdfid_oboid(UA,A),class(A).
owl2_model:objectProperty(UA) :- rdfid_oboid(UA,A),property(A).
owl2_model:individual(UA) :- rdfid_oboid(UA,A),inst(A).
owl2_model:subClassOf(UA,UB) :-
	rdfid_oboid(UA,A),rdfid_oboid(UB,B),subclass(A,B).
owl2_model:subClassOf(UA,Expr):-
	rdfid_oboid(UA,A),
	restriction(A,P,B),
	pval_expr(P,B,Expr).

owl2_model:equivalentClasses([UA,intersectionOf([UG|Restrs])]) :-
	rdfid_oboid(UA,A),
	rdfid_oboid(UG,G),
        genus(A,G),
        findall(Restr,(differentium(A,P,B)),pval_expr(P,B,Restr)),Restrs).

%pval_expr(card(RC,N),To,exactCardinality(N,R,To)) :- obolog_db:all_some(RC,R),!.
pval_expr(P,B,someValuesFrom(UA,UB)) :- % GUESS existential
	rdfid_oboid(UB,B),
	rdfid_oboid(UP,P),
        !.
pval_expr(R,To,_) :- throw(pval_expr('cannot translate ~w ~w',[R,To])).
