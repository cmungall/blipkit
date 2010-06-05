:- module(pkb_ilp,
	  [
	   write_all/2,
	   write_all_examples/0,
	   write_all_examples/2,
	   write_all_facts/0
	   ]).

:- use_module(pkb_db).
:- use_module(library(thea2/owl2_model)).
:- use_module(library(thea2/owl2_basic_reasoner)).


write_all(M,D) :-
	write_all_examples(M,D),
	write_all_facts.

write_all_facts :-
	setof(X,ilp_fact(X),Xs),
	length(Xs,Len),
	format(user_error,'num_facts: ~w~n',[Len]),
	maplist(write_fact,Xs).

write_all_examples :-
	write_all_examples('http://ccdb.ucsd.edu/PKB/1.0/PKB.owl#nlx_organ_20090203_587', % PS19
			   'http://ontology.neuinfo.org/NIF/Dysfunction/NIF-Dysfunction.owl#birnlex_2092'). % 'Alzheimers disease'

write_all_examples(ML,DL) :-
	labelAnnotation_value(M,ML),
	labelAnnotation_value(D,DL),
	!,
	setof(Dir-model_of(M2,D),model_of(M,M2,D,Dir),Xs),
	maplist(write_fact,Xs).
write_all_examples(M,D) :-
	setof(Dir-model_of(M2,D),model_of(M,M2,D,Dir),Xs),
	length(Xs,Len),
	format(user_error,'num_facts: ~w~n',[Len]),
	maplist(write_fact,Xs).

model_of(M,M,_,+).
model_of(_,M,D,+) :-
	orgtype_disease(M,D).

model_of(M1,M2,D,-) :-
	organism(M2),
	M2\=M1,
	\+orgtype_disease(M2,D).

orgtype_disease(M,D) :-
	organism_disease(M,D).
%orgtype_disease(M,D) :-
%	organism_disease(M1,D),
%	entailed(classAssertion(M,M1)),
%	atom(M).


ilp_fact(restriction(X,R,Y)) :-
	subClassOf(X,someValuesFrom(R,Y)),
	atom(X),
	atom(Y),
	X\=Y.
ilp_fact(subclass(X,Y)) :-
	subClassOf(X,Y),
	atom(X),
	atom(Y),
	X\=Y.

ilp_fact(organism(X)) :-
	organism(X).
ilp_fact(class(X)) :-
	class(X).
%	organism_phenotype(_,P),
%	class_quad_aspect(X,P,_).
%ilp_fact(organism_phenotype_quad(O,E,D,Q,W)) :-
%	organism_phenotype_quad(O,(E,D,Q,W)).
ilp_fact(phenotype(P2)) :-
	organism_phenotype(_,P),
	phenotype_safe(P,P2).
ilp_fact(organism_phenotype(O,P2)) :-
	organism_phenotype(O,P),
	phenotype_safe(P,P2).
ilp_fact(Fact) :- 
	organism_phenotype(_,P),
	phenotype_quad(P,PQ),
	phenotype_safe(P,P2),
	class_quad_aspect(C,PQ,A),
	%entailed(subClassOf(C,C2)),
	%atom(C2),
	C\='-',
	aspect_pred(A,P2,C,Fact).

aspect_pred(q,P,C,phenotype_quality(P,C)).
aspect_pred(e,P,C,phenotype_bearer(P,C)).
aspect_pred(d,P,C,phenotype_depends(P,C)).
aspect_pred(a,P,C,phenotype_context(P,C)).


phenotype_safe((E1,Q1,D1,W1),P) :-
	!,
	truncurl(E1,E),
	truncurl(Q1,Q),
	truncurl(D1,D),
	truncurl(W1,W),
	concat_atom([E,Q,D,W],P).
phenotype_safe(P,P).

write_fact((+)-X) :-
	!,
	write_fact(X).

write_fact((-)-X) :-
	!,
	format(':- '),
	write_fact(X).

write_fact(X) :-
	X=..[P|L],
	maplist(id2label,L,L2),
	X2=..[P|L2],
	!,
	format('~q.~n',[X2]).
write_fact(X) :- throw(fact(X)).

%:- dynamic cached/2.
%id2label(X,L) :- cached(X,L),!.
%id2label(X,L) :- !,gensym(f,L),assert(cached(X,L)).



id2label(X,L) :- labelAnnotation_value(X,V),!,truncurl(X,Base),trunclab(V,V1),concat_atom([Base,'-',V1],L).
id2label(X,X).

trunclab(X,L) :- sub_atom(X,0,10,_,L),!.
trunclab(X,X).

truncurl(U,X) :- concat_atom([_,X],'#',U),!.
truncurl(X,X).


