:- module(pkb_to_sim_via_ontol,
	  [
	   organism_att/2,
	   organism_closure/2
	   ]).

:- use_module(bio(simmatrix)).
:- use_module(bio(tabling)).
:- use_module(bio(index_util)).
:- use_module(bio(pkb_db)).
:- use_module(bio(ontol_db)).
:- use_module(bio(ontol_entailment_basic)).
:- use_module(bio(metadata_db)).

simmatrix:generate_term_indexes_hook(organism_phenotype_simple) :-
	generate_term_indexes(O,A,
			      pkb_to_sim_via_ontol:organism_closure(O,A)).
	
organism_att(O,A) :-
	organism_phenotype(O,P),
	phenotype_property_value(P,_,A).
organism_att(O,P) :-
	organism_phenotype(O,P),
	class(P).

organism_closure(O,A) :-
	setof(A1,organism_att(O,A1),A1s),
	member(A1,A1s),
	ontol_db:bf_parentRT(A1,A), % TODO - incl g/d
	allowed(A).

allowed(A) :- id_idspace(A,S),\+ss(S).

ss('MP').
ss('HP').
ss('FMA').
ss('MA').
ss('ZFA').


	