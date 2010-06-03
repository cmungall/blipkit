:- use_module(pkb_db).
:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).
:- use_module(bio(metadata_nlp)).

:- use_module(bio(index_util)).
:- use_module(bio(tabling)).
:- use_module(bio(relation_closure)).

:- multifile metadata_nlp:exclude_entity/1.

metadata_nlp:exclude_entity(C) :-
	\+ is_phenoclass(C).

create_vpo_index :-
	create_vpo_index('vpo_cache.pro').

create_vpo_index(File) :-
	table_pred(ontol_db:subclassT/2),
	table_pred(metadata_nlp:term_token_stemmed/3),
	materialize_index(metadata_nlp:entity_nlabel_scope_stemmed(1,1,0,0)),
	materialize_indexes_to_file([pheno_cdef(1,1),
				     newclass_cdef_xref(1,1,1),
				     newclass_cdef(1,1),
				     newclass_xref(1,1),
				     abduced_subclass(1,1),
				     abduced_subclassT(1,1),
				     abduced_equiv(1,1),
				     equivset(1),
				     canonical_equiv(1,1),
				     abduced_subclass_primary(1,1),
				     abduced_subclass_nr(1,1),
				     abduced_name(1,0)
				    %attribute_pair_LCS(1,0,0) % optional?
				    ],
				    File).

new_axiom(class(X)) :- newclass(X),\+is_secondary(X).
new_axiom(subclass(X,Y)) :- abduced_subclass_nr(X,Y).
new_axiom(subclass(X,Y)) :-
	newclass(X),
	\+is_secondary(X),
	\+ abduced_subclass_nr(X,_),
	newroot(Y),
	X\=Y.
new_axiom(genus(C,G)) :-
	newclass(C),\+is_secondary(C),
	newclass_cdef(C,cdef(G,_)).
new_axiom(differentium(C,R,X)) :-
	newclass(C),\+is_secondary(C),
	newclass_cdef(C,cdef(_,DL)),
	member(R=X,DL).
	
%new_axiom(subclass(X,Y)) :- abduced_equiv(X,Y), X@<Y.
new_axiom(metadata_db:entity_xref(C,X)) :-
	newclass_xref(C,X),
	\+is_secondary(C).
new_axiom(metadata_db:entity_xref(C,X)) :-
	canonical_equiv(C,E),
	newclass_xref(E,X).
new_axiom(metadata_db:entity_label(C,phenotype)) :-
	newroot(C).
new_axiom(metadata_db:entity_label(C,N)) :-
	abduced_name(C,N),
	\+is_secondary(C),
	\+ ((abduced_name(C,N2),
	     N2 @> N)). % lc precedence
new_axiom(metadata_db:entity_resource(X,metapheno)) :-
	newclass(X),\+is_secondary(X).

abduced_name(C,N) :-
	newclass_xref(C,X),
	entity_label(X,N).

% A < B if can be inferred from cdef
abduced_subclass(A,B) :-
	newclass_cdef(A,AX),
	newclass_cdef(B,BX),
	A\=B,
	subclassXT(AX,BX),
	debug(foo,'~w',[subclassXT(AX,BX)]).

	
% A < B if A and B are derived from classes that stand in a subclass relationship
abduced_subclass(A,B) :-
	newclass_xref(A,AX),
	newclass_xref(B,BX),
	subclassRT(AX,BX).

% closure
abduced_subclassT(A,B) :-
	node_relation_closure(A,abduced_subclass,Ancs),
	member(B,Ancs),
	% irreflexive unless truly equivalent
	(   A=B
	->  abduced_subclass(A,B)
	;   true).

% no cycles
abduced_subclass_primary(A,B) :-
	abduced_subclassT(A,B),
	\+ is_secondary(A),
	\+ is_secondary(B).

abduced_subclass_nr(A,B) :-
	abduced_subclass_primary(A,B),
	A\=B,
	\+ ((abduced_subclass_primary(A,B2),
	     B2\=A,
	     B2\=B,
	     abduced_subclass_primary(B2,B))).


abduced_equiv(A,B) :-
	abduced_subclassT(A,B),
	A\=B,
	abduced_subclassT(B,A).

% for any set formed by equivalent pairs,
% choose one member as the 'canonical'
canonical_equiv(Canonical,X) :-
	equivset(Es),
	Es=[Canonical|Rest],
	member(X,Rest).

	/*
	setof(E,abduced_equiv(Canonical,E),L),
	setof(E,(   member(E,L)
		;   E=Canonical),[X|_]),
	X\=Canonical.
	  */

xxequivset(Es) :-
	setof(E,abduced_equiv(C,E),L),
	setof(E,(   member(E,L)
		;   E=C),Es).

equivset(L) :-
	abduced_equiv(A,_),
	expand_equivset([A],[],[],L).
	
expand_equivset([ID|IDs],DoneIDs,Ancs,AncsFinal) :-
	setof(XID,abduced_equiv(ID,XID),Parents),
	!,
	ord_union(Parents,IDs,U),
	sort(DoneIDs,DoneIDsSorted),
	ord_subtract(U,DoneIDsSorted,NextIDs),
	ord_union(Ancs,Parents,AncsNew),
	expand_equivset(NextIDs,[ID|DoneIDsSorted],AncsNew,AncsFinal).
expand_equivset([ID|IDs],DoneIDs,Ancs,AncsFinal) :-
	!,
	expand_equivset(IDs,[ID|DoneIDs],Ancs,AncsFinal).
expand_equivset([],_,Ancs,Ancs).


is_secondary(X) :-
	canonical_equiv(_,X).

	


newroot('NEW:1').
newclass(X) :- newroot(X).
newclass(C) :- newclass_xref(C,_).


%% newclass_cdef_xref(ID,CDef,X)
% newclass for every distinct rewritten logical definition
newclass_cdef_xref(ID,CDef,X) :-
	pheno_cdef(X,CDef),
	atom(X),
	debug(foo,'making id for ~w',[CDef]),
	cdef_make_id(CDef,ID).

newclass_cdef(ID,CDef) :-
	pheno_cdef(X,CDef),
	\+ atom(X),
	cdef_make_id(CDef,ID).
newclass_cdef(ID,CDef) :-
	newclass_cdef_xref(ID,CDef,_).

newclass_xref(ID,X) :-
	newclass_cdef_xref(ID,_,X).

% newclass by exact string match
newclass_xref(ID,A) :-
	class(A),
	is_phenoclass(A),
	entity_nlabel_scope_stemmed(A,N,_,true),
	entity_nlabel_scope_stemmed(B,N,_,true),
	A\=B,
	is_phenoclass(N),
	order_pair(A,B,L),
	cdef_make_id(union(L),ID).

order_pair(A,B,[A,B]) :- A @< B,!.
order_pair(A,B,[B,A]).



cdef_make_id(union(L),ID) :-
	!,
	maplist(cdef_make_id,L,Xs),
	concat_atom(['NEW:union_'|Xs],'_',ID).
cdef_make_id(cdef(G,DL),ID) :-
	!,
	cdef_make_id(G,G2),
	maplist(cdef_make_id,DL,Xs),
	concat_atom(['NEW:cdef',G2|Xs],'_',ID).
cdef_make_id(R=X,A) :-
	!,
	cdef_make_id(R,R2),
	cdef_make_id(X,Y),
	concat_atom([R2,Y],'=',A).
cdef_make_id(X,A) :-
	!,
	concat_atom(L,':',X),
	concat_atom(L,'_',A).

/*
cdef_id_axioms(CDef,ID,L) :-
	setof(P,pheno_cdef(P,CDef),Ps),
	!,
	cdef_make_id(CDef,ID),
	foo.
*/

pheno_cdef_direct(P,CDef) :-
	class_cdef(P,CDef),
	id_idspace(P,Ont),
	pheno_ont(Ont).

pheno_cdef_direct(P,CDef) :-
	phenotype_quad_cdef(P,_,CDef).

pheno_cdef(P,CDef) :-
	pheno_cdef_direct(P,CDef1),
	normalize_cdef(CDef1,CDef),
	valid_cdef(CDef).


normalize_cdef(cdef(Ab,DL),cdef('PATO:0000001',DL)) :-
	ab(Ab),!.
normalize_cdef(cdef(G,DL),cdef(G,DL3)) :-
	normalize_diffs(DL,DL2),
	fix_rels_in_diffs(DL2,DL3).

valid_cdef(cdef(_,[_|_])).


fix_rels_in_diffs(L1,['OBO_REL:inheres_in'=X|L2]) :-
	select('OBO_REL:inheres_in_part_of'=X,L1,L2),
	\+ member('OBO_REL:inheres_in'=_,L2),
	!.
fix_rels_in_diffs(L,L).
	
normalize_diffs([],[]).
normalize_diffs([_=Ab|DL],DL2) :-
	ab(Ab),
	!,
	normalize_diffs(DL,DL2).
normalize_diffs([R=X|DL],[R=X2|DL2]) :-
	normalize_class(X,X2),
	normalize_diffs(DL,DL2).

normalize_class(X,U) :-
	entity_xref(U,X),
	id_idspace(U,'UBERON'),
	!.
normalize_class(X,X).

is_phenoclass(C) :-
	id_idspace(C,Ont),
	pheno_ont(Ont).

pheno_ont('MP').
pheno_ont('HP').
pheno_ont('WP').

ab('PATO:0000460').
	
/*

  notes - hypotelorism doesnt grab children due to different xp defs
  
  */



