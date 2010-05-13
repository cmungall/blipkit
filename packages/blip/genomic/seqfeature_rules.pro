/* -*- Mode: Prolog -*- */
:- module(seqfeature_rules,[]).

:- use_module(bio(ontol_reasoner)).

/**

  how do we treat relations in SO?
   part_of as subsequence_of?
   sequence_of - S and C
   subsequence_of - S and S

*/

% bridge from seqfeature_db
% we create facts such as intron(i1)
% note that here i1 is a type
predicate(T,[F]) <- feature_type(F,T).
predicate(T,[F],Time) <- inst_of_at(F,T,Time).

% skolemize locations
% what about non-directional features?
has_start(X,Src-P) <- featureloc(X,Src,P,_).
has_end(X,Src-P) <- featureloc(X,Src,_,P).

% GRCC
% distinguish from normal RCC
% adjacency in 3d does not apply sequence adjacency: eg RNA structure
adjacent_to(L,R) <- upstream_adjacent_to(L,R).
adjacent_to(L,R) <- downstream_adjacent_to(L,R).
upstream_adjacent_to(L,R) <-> downstream_adjacent_to(R,L).

upstream_adjacent_to(L,R) <- has_end(R,PR),has_start(L,LP),coincident(LP,RP).

coincident(X,X).

% this will build a lot of pairs...
intersects(X,Y) <- has_start(X,XP),has_range(Y,YR),between(XP,YR).
intersects(X,Y) <- has_start(Y,YP),has_range(X,XR),between(YP,XR).
intersects(X,Y) <- has_end(X,XP),has_range(Y,YR),between(XP,YR).

% todo: ground? only invoke when asked? normal horn clause?
between(Src-P,range(Src,A,B)) :- P > A, P < B.
between(Src-P,range(Src,A,B)) :- P > B, P < A.

% we name these such that they are more specific than topological RCC relations
% optimize?
% TODO: check: incl adjacency?
sequentially_spatially_disjoint(X,Y) <- not(intersects(X,Y)).

upstream_spatially_disjoint(L,R) <- has_end_on(L,LP,Src),has_start_on(R,RP,Src),LP < RP.
downstream_spatially_disjoint(L,R) <- has_end_on(L,LP,Src),has_start_on(R,RP,Src),LP > RP.


% todo: time
% intron can be cut loose
intron(I),
has_start(I,X1E),
has_end(I,X2S)
<-
exon(X1),
exon(X2),
cotranscribed(X1,X2),
has_end(X1,X1E),
has_start(X2,X2E).

has_start(R,P) <- upstream_adjacent_to(L,R),has_end(L,P).
has_end(R,P) <- upstream_adjacent_to(L,R),has_start(L,P).

% all introns have 2 exons adjacent to them
% todo: existentials
exists(X),exon(X), upstream_adjacent_to(I,X) <- intron(I).
exists(X),exon(X), upstream_adjacent_to(X,I) <- intron(I).

% projection
transcribed_from(T,G),has_start_on(X,P,T) -> has_projected_start_on(X,P,G).

% relation to continuants
chromosome_sequence(CS) -> exists(C),inst_of(C,'GO:chromosome'),sequence_of(CS,C),

% if a chromosome is in a nucleus, subsequences of the chromosome sequence must be in the nucleus
% todo: time. RNA may be transcribed in the nucleus but then transported to a dendrite
sequence_of(S,C),subsequence_of(S2,S),sequence_of(S2,C2),located_in(C,L) -> located_in(C2,L).
sequence_of(S,C),subsequence_of(S2,S),sequence_of(S2,C2),part_of(C,P) -> part_of(C2,P).

rcc_disjoint_from(X,Y) -> not(located_in(X,Y)).

