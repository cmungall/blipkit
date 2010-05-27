:- module(owl2_to_sim,
	  []).

:- use_module(bio(simmatrix_multiset),[]).
:- use_module(bio(tabling)).
:- use_module(bio(index_util)).

:- use_module(library('thea2/owl2_basic_reasoner')).
:- use_module(library('thea2/owl2_model'),[equivalent_to/2,subClassOf/2,subPropertyOf/2]).

simmatrix_multiset:subsumed_by(X,Y) :-
	%entailed(subClassOfReflexive(X,Y)),
	subClassOfRT(X,Y),
	\+ exclude(Y).

exclude(Class) :-
	atom(Class),
	sub_atom(Class,0,_,_,'http://www.ifomis.org').

:- multifile simmatrix_multiset:index_hook/1.
simmatrix_multiset:index_hook(reasoner) :-
	%table_pred(owl2_basic_reasoner:entailed/1).
	materialize_index(owl2_model:equivalent_to(1,1)),
	table_pred(owl2_to_sim:subClassOfT/3), % helps?
	table_pred(owl2_to_sim:subClassOfT/2).


subClassOfRT(A,A) :- owl2_model:class(A).
subClassOfRT(A,B) :- subClassOfT(A,B).

subClassOfT(A,B) :- subClassOfT(A,B,x).

subClassOfT(A,B,x) :- subClassOf(A,B).


% potential cycles?
subClassOfT(A,B,x) :- equivalent_to(A,B). 


subClassOfT(A,B,Depth) :-
    subClassOf(A,Z),
    subClassOfT(Z,B,Depth).

subClassOfT(A,B,x(Depth)) :-
    nonvar(A),
    A=someValuesFrom(RA,AX),
    nonvar(RA),
    nonvar(AX),
    B=someValuesFrom(RB,BX),
    subPropertyOfRT(RA,RB),
    subClassOfT(AX,BX,Depth),
    depth_check(Depth).

subClassOfT(A,B,x(Depth)) :-
    nonvar(B),
    B=someValuesFrom(RA,BX),
    nonvar(RA),
    nonvar(BX),
    A=someValuesFrom(RB,AX),
    subPropertyOfRT(RA,RB),
    subClassOfT(AX,BX,Depth),
    depth_check(Depth).    

subClassOfT(A,B,x(Depth)) :-
    var(B),
    A=someValuesFrom(R,AX),
    nonvar(R),
    nonvar(AX),
    transitiveProperty(R),
    subClassOfT(AX,someValuesFrom(R,B),Depth),
    depth_check(Depth).


subPropertyOfRT(A,B) :-
      subPropertyOfT(A,B).
subPropertyOfRT(A,A) :- objectProperty(A).


subPropertyOfT(A,B) :-
	subPropertyOf(A,B).

subPropertyOfT(A,B) :-
	subPropertyOf(A,Z),
	subPropertyOfT(Z,B).


depth_check(Depth) :-
	Depth\=x(x(_)).
