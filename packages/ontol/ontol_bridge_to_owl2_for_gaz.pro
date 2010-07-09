/* -*- Mode: Prolog -*- */

:- module(ontol_bridge_to_owl2_for_gaz,
          [
           optimize_gaz/0
           ]).

:- use_module(bio(ontol_bridge_to_owl2_and_iao)).
:- use_module(bio(ontol_bridge_to_owl2),[uri_oboid/2]).
:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).
:- use_module(library('thea2/owl2_model'),
	      []).

native_to_literal(Term,literal(Term)):- number(Term),!.
native_to_literal(Term,literal(lang(en,Term))).

% ----------------------------------------
% OPTIMIZE
% ----------------------------------------

optimize_gaz :-
        ensure_loaded(bio(index_util)),
        materialize_index(ontol_db:subclass(1,1)),
        materialize_index(ontol_db:def_xref(1,1)).

%        materialize_index(rewrite_as_class(1)),
%        materialize_index(rewrite_as_instance(1)).
                         %materialize_index(ontol_bridge_to_owl2_for_gaz:
        

% ----------------------------------------
% REWRITE
% ----------------------------------------

:- initialization(abolish(owl2_model:class/1), now).
:- initialization(abolish(owl2_model:subClassOf/2), now).

%:- abolish(owl2_model:class/1).
%:- abolish(owl2_model:subClassOf/2).

:- discontiguous owl2_model:subClassOf/2.

% ----------------------------------------
% TRANSLATION RULES
% ----------------------------------------

rewrite_as_class(A) :-
        \+ \+ subclass(_,A).    % child does exist: class        

rewrite_as_instance(A) :-
        \+ subclass(_,A).       % child does not exist: individual        

% ----------------------------------------
% MIREOT ENVO
% ----------------------------------------

cl_label('http://purl.obolibrary.org/obo/ENVO_00000000','geographic feature').

owl2_model:class(P) :- cl_label(P,_).
owl2_model:annotationAssertion('http://www.w3.org/2000/01/rdf-schema#label',P,literal(lang(en,N))) :- cl_label(P,N).

% ----------------------------------------
% Wikipedia
% ----------------------------------------

entity_dbpedia(A,URI) :-
        def_xref(A,DX),
        atom_concat('url:http://en.wikipedia.org/wiki/',Local,DX),
        \+ ((def_xref(B,DX),
             B\=A)),
        \+ sub_atom(Local,_,_,_,'#'),
        atom_concat('http://dbpedia.org/resource/',Local,URI).

owl2_model:sameIndividual([UA,UB]) :-
	uri_oboid(UA,A),
        entity_dbpedia(A,UB),
        rewrite_as_instance(A).  % child does not exist: instance

owl2_model:equivalentClasses([UA,UB]) :-
	uri_oboid(UA,A),
        entity_dbpedia(A,UB),
        rewrite_as_class(A).  % child does not exist: instance

% ----------------------------------------
% TREAT GAZ AS INSTANCES
% ----------------------------------------

% we can't reuse ontol_bridge_to_owl2, need to hack classes as
% instances

% re-root nodes
owl2_model:subClassOf(UA,'http://purl.obolibrary.org/obo/ENVO_00000000') :-
	uri_oboid(UA,A),
        class(A),
        rewrite_as_class(A),
        \+ subclass(A,_).

% is_a --> subClassOf when the child is not a leaf
owl2_model:subClassOf(UA,UB) :-
	uri_oboid(UA,A),uri_oboid(UB,B),
        subclass(A,B),
        rewrite_as_class(A). % child exists

% is_a --> subClassOf when the child is a leaf
owl2_model:classAssertion(UC,UI) :-
	uri_oboid(UC,C),uri_oboid(UI,I),
        subclass(I,C),
        rewrite_as_instance(I).

% instance-instance
owl2_model:propertyAssertion(UP,UA,UB) :-
	uri_oboid(UA,A),uri_oboid(UB,B),
        uri_oboid(UP,P),
        restriction(A,P,B),
        rewrite_as_instance(A), % child does not exist: instance
        rewrite_as_instance(B). % child does not exist: instance

% class-class;
% very few of these
owl2_model:subClassOf(UA,someValuesFrom(UP,UB)) :-
	uri_oboid(UA,A),uri_oboid(UB,B),
        uri_oboid(UP,P),
        restriction(A,P,B),
        rewrite_as_class(A),    % child does exist: class
        rewrite_as_class(B).    % child does exist: class

% e.g. located place Scotland located_in value scotland
owl2_model:subClassOf(UA,hasValue(UP,UB)) :-
	uri_oboid(UA,A),uri_oboid(UB,B),
        uri_oboid(UP,P),
        restriction(A,P,B),
        rewrite_as_class(A),    % child does exist: class
        rewrite_as_instance(B).    % child does not exist: instance

/*

  sanity check; 

  blip-findall -r gaz "restriction(A,R,B),subclass(_,A),\+ \+subclass(_,B)" -select "r(A,R,B)" 

*/