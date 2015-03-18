:- module(curation_bridge_to_owl2,
	  [
	   inferred_annot/5
	   ]).

:- use_module(curation_db).
:- use_module(bio(ontol_db)).

:- use_module(bio(ontol_bridge_to_owl2_and_iao)).
:- use_module(bio(ontol_bridge_to_owl2),[uri_oboid/2]).
:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).
:- use_module(library('thea2/owl2_model'),
	      []).

native_to_literal(Term,literal(Term)):- number(Term),!.
native_to_literal(Term,literal(lang(en,Term))).

% ----------------------------------------
% MIREOT from ChEBI
% ----------------------------------------

cl_label('http://purl.obolibrary.org/obo/CHEBI_33695','information macromolecule').

owl2_model:class(P) :- cl_label(P,_).
owl2_model:annotationAssertion('http://www.w3.org/2000/01/rdf-schema#label',P,literal(lang(en,N))) :- cl_label(P,N).

% each annotation corresponds 1:1 to a gene product *instance*
owl2_model:namedIndividual(UA) :-
        uri_oboid(UA,A),
        curation(A).
owl2_model:classAssertion('http://purl.obolibrary.org/obo/CHEBI_33695',UA) :-
        uri_oboid(UA,A),
        curation(A).

% we have an isoform - annot ID denotes gene product instance, direct class assertion
owl2_model:classAssertion(GP_Class_URI,UA) :-
        uri_oboid(UA,A),
        uri_oboid(GP_Class_URI,GP_Class),
        curation_isoform(A,GP_Class).

% we do not have the isoform - annot ID denotes gene product that derives from gene
owl2_model:classAssertion(hasValue(UP,Gene_Class_URI),UA) :-
        uri_oboid(UA,A),
        P=encoded_by,
        uri_oboid(UP,P),
        uri_oboid(Gene_Class_URI,Gene_Class),
        curation_statement(A,Gene_Class,_,_),
        \+ curation_isoform(A,_).

% make generalized link between gene class and gene product class
owl2_model:subClassOf(GP_Class_URI,hasValue(encodedBy,Gene_URI)) :-
        uri_oboid(Gene_URI,Gene),
        uri_oboid(GP_Class_URI,GP_Class),
        curation_isoform(A,GP_Class),
        curation_statement(A,Gene,_,_). % uniquify

% the annotation: a statement about the function/location of an instance
owl2_model:classAssertion(someValuesFrom(UP,Ont_Class_URI),UA) :-
        uri_oboid(Ont_Class_URI,Ont_Class),
        uri_oboid(UA,A),
        uri_oboid(UP,P),
        curation_statement(A,_,Rel,Ont_Class),
        infer_property(A,Ont_Class,Rel,P).







%% inferred_annot(?G,?R,+X,?Q,?Ev) is nondet
inferred_annot(G,R,X,some,Ev) :-
	some(G,R,X,Ev).
inferred_annot(G,R,X,integral_to,Ev) :-
	integral_to(G,R,X,Ev).

:- discontiguous some/4.
:- discontiguous integral_to/4.


% ----------------------------------------
% ASSERTED
% ----------------------------------------

some(G,R,X,A) :-
	curation_statement(A,G,R1,X),
	infer_property(A,X,R1,R).
integral_to(G,R,X,A) :-
	curation_statement(A,G,R1,X),
	infer_property(A,X,R1,R),
	curation_qualifier(A,integral_to,true).



% ----------------------------------------
% RULES
% ----------------------------------------


%%%

some(G,R,X,E) :- subclass(X2,X),some(G,R,X2,E).

% transitive part_of
some(G,part_of,C,E) :- parent(C2,part_of,C),some(G,part_of,C2,E).

some(G,is_active_participant_in,P,E) :- parent(P2,part_of,P),some(G,is_active_participant_in,P2,E).
some(G,is_active_participant_in,P,E) :- parent(P2,part_of,P),some(G,executes,P2,E).

% regulates
some(G,regulates,P,E) :- parent(P2,regulates,P),some(G,executes,P2,E).
some(G,negatively_regulates,P,E) :- parent(P2,negatively_regulates,P),some(G,executes,P2,E).
some(G,positively_regulates,P,E) :- parent(P2,positively_regulates,P),some(G,executes,P2,E).

some(G,regulates,P,E) :- parent(P2,regulates,P),some(G,is_active_participant_in,P2,E).
some(G,negatively_regulates,P,E) :- parent(P2,negatively_regulates,P),some(G,is_active_participant_in,P2,E).
some(G,positively_regulates,P,E) :- parent(P2,positively_regulates,P),some(G,is_active_participant_in,P2,E).

some(G,regulates,P,E) :- parent(P2,part_of,P),some(G,regulates,P2,E).

some(G,indirectly_regulates,P,E) :- parent(P2,regulates,P),some(G,regulates,P2,E).

some(G,indirectly_negatively_regulates,P,E) :- parent(P2,positively_regulates,P),some(G,negatively_regulates,P2,E).
some(G,indirectly_positively_regulates,P,E) :- parent(P2,positively_regulates,P),some(G,positively_regulates,P2,E).
some(G,indirectly_positively_regulates,P,E) :- parent(P2,positively_regulates,P),some(G,positively_regulates,P2,E).

% INTEGRAL TO

/*
integral_to(G,part_of,C_Big,E) :- parent(C_Big,has_part,C_Small),integral_to(G,part_of,C_Small,E).
integral_to(G,is_active_participant_in,C_Big,E) :- parent(C_Big,has_part,C_Small),integral_to(G,is_active_participant_in,C_Small,E).

% reverse propagation
integral_to(G,R,C_Specific,E) :- subclass(C_Specific,C_Generic),integral_to(G,R,C_Generic,E).
*/




%%%

%% infer_property(+Annot,+Class,+InRel,?ClassifiedRel) :-
infer_property(I,X,R1,R) :-
	infer_property1(I,X,R1,R),
	!.
infer_property(_,_,R,R) :- !.

infer_property1(_,X,_,part_of) :-
	belongs(X,cellular_component).
infer_property1(_,X,_,is_active_participant_in) :-
	belongs(X,biological_process).
infer_property1(_,X,_,executes) :-
	belongs(X,molecular_function).

/*

  Annotation(
    subject(g)
    target(p)
  )
  ==>
  exists some molecule m, m encoded_by g, m involved_in p
  ClassAssertion( (Protein and encoded_by value g and involved_in some p) obs)

  to get:
  
  GO
   GO
   GO
    GO
     p

  1. create hierarchy of encodes_protein_that_involved_in <p>
  
  ClassAssertion( (Gene and gene_for some p) g)
  ClassAssertion( (Gene and encodes some (Mol and involved_in some p) g) )

  Annotation(
    subject(g)
    target(p)
    isoform(m)
  )
  ==>
  ClassAssertion( (Gene and gene_for some p) g)
  
*/