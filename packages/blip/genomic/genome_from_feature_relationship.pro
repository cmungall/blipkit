/* -*- Mode: Prolog -*- */

:- module(genome_from_feature_relationship,
          []).

:- use_module(seqfeature_db).
:- use_module(genome_db,
	      [exon/1,
	       gene/1,
	       transcript/1,
	       cds/1,
	       regulatory_region/1,
	       intron/1]).
:- use_module(bio(ontol_db)).

genome_db:gene_transcript(G,T):- feature_relationship(T,G),gene(G),transcript(T).
genome_db:exon_transcript(X,T):- feature_relationship(X,T),exon(X),transcript(T).

% Now inferred in grule/exon_transcript_order
% genome_db:exon_transcript_order(X,T,Rank) :- feature_relationship(X,T,_,Rank),exon(X),transcript(T).


% TODO: chado vs gff3
genome_db:transcript_cds(T,C):- feature_relationship(C,T),cds(C),transcript(T).

genome_db:regulates_gene(R,G):- regulatory_region(R),feature_relationship(R,G).

% POS

genome_db:exon_dnaseq_pos(X,Seq,B,E,Str) :- exon(X),featureloc(X,Seq,B,E,Str).
genome_db:transcript_dnaseq_pos(X,Seq,B,E,Str) :- transcript(X),featureloc(X,Seq,B,E,Str).
genome_db:intron_dnaseq_pos(X,Seq,B,E,Str) :- intron(X),atom(X),featureloc(X,Seq,B,E,Str).
genome_db:cds_dnaseq_pos(X,Seq,B,E,Str) :- cds(X),featureloc(X,Seq,B,E,Str).

