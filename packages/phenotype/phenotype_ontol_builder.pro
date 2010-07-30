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
	materialize_index(ontol_db:subclassRT(1,1)),
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
				     abduced_name(1,1),
				     abduced_synonym(1,0,0)
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
new_axiom(metadata_db:entity_synonym_scope(C,N,Sc)) :-
	abduced_synonym(C,N,Sc),
	\+is_secondary(C),
	\+ new_axiom(metadata_db:entity_label(C,N)).
new_axiom(metadata_db:entity_synonym(C,N)) :-
	new_axiom(metadata_db:entity_synonym_scope(C,N,_)).
new_axiom(metadata_db:entity_resource(X,metapheno)) :-
	newclass(X),\+is_secondary(X).

abduced_name(C,N) :-
	newclass_xref(C,X),
	entity_label(X,N).
abduced_synonym(C,N,Sc) :-
	newclass_xref(C,X),
	entity_synonym_scope(X,N,Sc).
abduced_synonym(C,N,exact) :-
	abduced_name(C,N).
	
% A < B if A and B are derived from classes that stand in a subclass relationship
abduced_subclass(A,B) :-
	newclass_xref(A,AX),
	newclass_xref(B,BX),
	subclass(AX,BX). % we ensure that we have the full isa path, so we only need direct here


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

%% abduced_equiv(A,B)
% true if A and B are equivalent based on abduced subclass
abduced_equiv(A,B) :-
	abduced_subclassT(A,B),
	A\=B,
	abduced_subclassT(B,A). % cycle

abduced_equiv(A,B) :- % equiv cdefs
	newclass_cdef(A,X),
	newclass_cdef(B,X),
	A\=B.

%% canonical_equiv(?Canonical,?X)
% true if X and Canonical are equivalent, and Canonical is the canonical
% member of that equivset
%
% for any set formed by equivalent pairs,
% choose one member as the 'canonical'
canonical_equiv(Canonical,X) :-
	equivset(Es),
	Es=[Canonical|Rest],
	member(X,Rest).


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

	


newroot('UPHENO:1').
newclass(X) :- newroot(X).
newclass(C) :- newclass_xref(C,_).


%% newclass_cdef_xref(ID,CDef,X)
% newclass for every distinct rewritten logical definition
newclass_cdef_xref(ID,CDef,X) :-
	pheno_cdef(X,CDef),
	class(X),
	debug(foo,'making id for ~w',[CDef]),
	cdef_make_id(CDef,ID).

newclass_cdef(ID,CDef) :-
	pheno_cdef(X,CDef),
	\+ atom(X),
	cdef_make_id(CDef,ID).
newclass_cdef(ID,CDef) :-
	newclass_cdef_xref(ID,CDef,_).

% make a new class for any pair of classes with the same name
% this will have a union([A,B]) skolem ID
newclass_xref_pair(ID,A,B) :-
	class(A),
	is_phenoclass(A),
	entity_nlabel_scope_stemmed(A,N,_,true),
	entity_nlabel_scope_stemmed(B,N,_,true),
	A\=B,
	is_phenoclass(B),
	order_pair(A,B,L),
	cdef_make_id(union(L),ID).

% newclass for anything with a logical definition
newclass_xref_direct(ID,X) :-
	newclass_cdef_xref(ID,_,X).

% newclass by exact string match
newclass_xref_direct(ID,A) :-
	newclass_xref_pair(ID,A,_).

% ensure we include the closure of all referenced terms
newclass_xref_1(ID,X) :-
	newclass_xref_direct(ID,X).
newclass_xref_1(ID,Y) :-
	newclass_xref_direct(_,X),
	subclassRT(X,Y),
	\+ newclass_xref_direct(_,Y),
	cdef_make_id(union([Y]),ID).

newclass_xref(ID,X) :-
	setof(ID-X,newclass_xref_1(ID,X),Pairs),
	member(ID-X,Pairs).


order_pair(A,B,[A,B]) :- A @< B,!.
order_pair(A,B,[B,A]).



cdef_make_id(union(L),ID) :-
	!,
	maplist(cdef_make_id,L,Xs),
	concat_atom(['UPHENO:union_'|Xs],'_',ID).
cdef_make_id(cdef(G,DL),ID) :-
	!,
	cdef_make_id(G,G2),
	maplist(cdef_make_id,DL,Xs),
	concat_atom(['UPHENO:cdef',G2|Xs],'_',ID).
cdef_make_id(R=X,A) :-
	!,
	cdef_make_id(R,R2),
	cdef_make_id(X,Y),
	concat_atom([R2,Y],'_',A).
cdef_make_id(X,A) :-
	!,
	concat_atom(L,':',X),
	concat_atom(L,'_',A).
cdef_make_id(X,A) :-
	concat_atom(['OBO_REL',A],':',X),
	!.

pheno_cdef_direct(P,CDef) :-
	class_cdef(P,CDef),
	class(P), % exclude obsoletes
	id_idspace(P,Ont),
	pheno_ont(Ont).

pheno_cdef_direct(P,CDef) :-
	phenotype_quad_cdef(P,_,CDef).

pheno_cdef(P,CDef) :-
	pheno_cdef_direct(P,CDef1),
	normalize_cdef(CDef1,CDef),
	valid_cdef(CDef),
	\+ invalid_cdef(CDef).



normalize_cdef(cdef(Ab,DL),cdef('PATO:0000001',DL)) :-
	ab(Ab),!.
normalize_cdef(cdef(G,DL),cdef(G,DL3)) :-
	normalize_diffs(DL,DL2),
	fix_rels_in_diffs(DL2,DL3).

valid_cdef(cdef(_,[_|_])).

invalid_cdef(cdef(_,DL)) :- member(has_part=_,DL). % slows down reasoning and leads to odd hp errors


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



