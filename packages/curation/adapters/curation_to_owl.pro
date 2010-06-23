/* -*- Mode: Prolog -*- */
:- module(curation_to_owl,
          []).

:- use_module(bio(curation_db)).
:- use_module(library(thea2/owl2_model)).

% we treat each statement as an observed gene product / subject instance
owl2_model:classAssertion(C,I) :-
	curation_statement(I,C,_,_).
owl2_model:classAssertion(C,I) :-
	negative_curation_statement(I,C,_,_).
owl2_model:classAssertion(someValuesFrom(R,X),I) :-
	curation_statement(I,_,R1,X),
	infer_property(I,X,R1,R).
owl2_model:classAssertion(allValuesFrom(R,complementOf(X)),I) :-
	negative_curation_statement(I,_,R1,X),
	infer_property(I,X,R1,R).

% example: G integral_to C ==> all C has_part some G
% todo: scope by species
owl2_model:subClassOf(X,someValuesFrom(inverseOf(R),G)) :-
	curation_statement(I,_,R1,X),
	curation_qualifier(I,integral_to,true),
	infer_property(I,X,R1,R).

infer_property(I,X,R1,R) :-
	infer_property1(I,X,R1,R2),
	relation_uri(R2,R),
	!.
infer_property(_,_,R1,R) :-
	relation_uri(R1,R).

infer_property1(_,X,_,part_of) :-
	belongs(X,cellular_component).
infer_property1(_,X,_,is_active_participant_in) :-
	belongs(X,biological_process).
infer_property1(_,X,_,executes) :-
	belongs(X,molecular_function).

%relation_uri(part_of,'http://purl.obolibrary.org/obo/BFO_0000050').
relation_uri(part_of,'http://purl.obolibrary.org/obo/BFO_0000050') :- !.
relation_uri(X,X).





