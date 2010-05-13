:- module(pkb_export,
	  [export_fact/1]).


:- use_module(pkb_db).
:- use_module(library(thea2/owl2_model)).

export_fact(Ax) :-
	Ax=subClassOf(A,B),
	Ax,
	atom(A),
	atom(B).
export_fact(label(C,V)) :-
	labelAnnotation_value(C,V).
export_fact(label(C,V)) :-
	organism_label(C,V).
export_fact(organism_phenotype_tag_value(O,P,A,C)) :-
	organism_phenotype_quad(O,P,Q),
	class_quad_aspect(C,Q,A).
	