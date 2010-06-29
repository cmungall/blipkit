/* -*- Mode: Prolog -*- */

:- module(ontol_bridge_to_owl2_for_gaz,
          [
           ]).

:- use_module(bio(ontol_bridge_from_owl2_and_iao)).
:- use_module(bio(ontol_bridge_from_owl2)).
:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).
:- use_module(library('thea2/owl2_model'),
	      []).

native_to_literal(Term,literal(Term)):- number(Term),!.
native_to_literal(Term,literal(lang(en,Term))).

% ----------------------------------------
% REWRITE
% ----------------------------------------

:- abolish(owl2_model:class/1).
:- abolish(owl2_model:subClassOf/2).


% ----------------------------------------
% MIREOT ENVO
% ----------------------------------------

cl_label('http://purl.obolibrary.org/obo/ENVO_00000000','geographic feature').

owl2_model:class(P) :- cl_label(P,_).
owl2_model:annotationAssertion('http://www.w3.org/2000/01/rdf-schema#label',P,literal(lang(en,N))) :- cl_label(P,N).

% ----------------------------------------
% TREAT GAZ AS INSTANCES
% ----------------------------------------

% we can't reuse ontol_bridge_to_owl2, need to hack classes as
% instances

owl2_model:subClassOf(UA,'http://purl.obolibrary.org/obo/ENVO_00000000') :-
	uri_oboid(UA,A),
        class(A),
        \+ subclass(A,_).

owl2_model:subClassOf(UA,UB) :-
	uri_oboid(UA,A),uri_oboid(UB,B),
        subclass(A,B),
        \+ \+ subclass(_,A).

owl2_model:classAssertion(UA,UB) :-
	uri_oboid(UA,A),uri_oboid(UB,B),
        subclass(A,B),
        \+ \+ subclass(_,A).

owl2_model:propertyAssertion(UP,UA,UB) :-
	uri_oboid(UA,A),uri_oboid(UB,B),
        uri_oboid(UP,P),
        restriction(UA,UP,UB).


