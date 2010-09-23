:- use_module(pkb_db).
:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).
:- use_module(bio(metadata_nlp)).

:- use_module(bio(index_util)).
:- use_module(bio(tabling)).
:- use_module(bio(relation_closure)).

setup :-
	materialize_index(pclass_cdef/2).


quad_id((E,Q,D,W),ID) :-
	concat_atom([E,Q,D,W],'_',A),
	concat_atom(Toks,':',A),
	concat_atom(['AUTO:P'|Toks],'_',ID).

% TODO - do not rely on quads

% basic
pclass_cdef(P,D) :-
	phenotype_cdef(PQ,D),
	quad_id(PQ,P).

% make a class for the non-relational case
pclass_cdef(P,Def) :-
	phenotype_cdef((E,Q,D,W),_),
	D\='-',
	W='-',
	Def=cdef(Q,[inheres_in=E]),
	quad_id((E,Q,-,-),P).

% basic E case
pclass_cdef(P,Def) :-
	phenotype_cdef((E,_,_,_),_),
	Q='PATO:0000001',
	Def=cdef(Q,[inheres_in=E]),
	quad_id((E,Q,-,-),P).

ontol_db:class(C) :- pclass_cdef(C,_).

ontol_db:genus(C,G) :- pclass_cdef(C,cdef(G,_)).

ontol_db:differentium(C,R,Y) :- pclass_cdef(C,cdef(_,DL)),member(R=Y,DL).

metadata_db:entity_label(C,N) :-
	pclass_cdef(C,D),
	abn_label(D,Toks,[]),
	concat_atom(Toks,' ',N).
metadata_db:entity_resource(C,auto) :-
	pclass_cdef(C,_).


% abnormal <e> <attr>   # abnormal tail length
% abnormal <rel> to <d> in <e> # abnormal sensitivity towards light in eye
% <e> <qv> # tail long
% <e> <rel> <d>  # eye increased sensitivity to light

abn_label(cdef(G,DL)) -->
	{entity_partition(G,attribute_slim),
	 select(towards=T,DL,DL2)},
	!,
	[abnormal],
	q_label(G),
	anat_label(DL2),
	anat_label(T).

abn_label(cdef(G,DL)) -->
	{entity_partition(G,attribute_slim)},
	!,
	[abnormal],
	anat_label(DL),
	q_label(G).

abn_label(cdef(G,DL)) -->
	{select(towards=T,DL,DL2)},
	!,
	[abnormal],
	anat_label(DL2),
	q_label(G),
	anat_label(T).

abn_label(cdef(G,DL)) -->
	anat_label(DL),
	q_label(G).

anat_label(DL) -->
	{member(inheres_in_part_of=W,DL),
	 member(inheres_in=E,DL)},
	!,
	anat_label(W),
	anat_label(E).
anat_label(DL) -->
	{member(inheres_in_part_of=W,DL)},
	!,
	anat_label(W).
anat_label(DL) -->
	{member(inheres_in=E,DL)},
	!,
	anat_label(E).


anat_label(X) -->	c_label(X).
q_label(X) -->	c_label(X).

c_label(X) -->
	{entity_label(X,N)},
	!,
	[N].
c_label(X) -->
	[X].



% ----------------------------------------
% ZFIN GENES
% ----------------------------------------

gene_zpo(G,P) :-
        class_cdef(P,cdef(Q,DL)),
        member(inheres_in=E,DL),
        (   member(towards=D,DL)
        ;   D='-'),
        g2p(G,E,Q,_,D,_,_,_). % TODO - correct order
        
