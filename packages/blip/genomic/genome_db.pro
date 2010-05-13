/* -*- Mode: Prolog -*- */

:- module(genome_db,
          [
           transcript/1, exon/1, regulates_gene/2,
           gene/1,
           intron/1,
           cds_exon_pos_pair/5,
           start_codon/1,
           stop_codon/1,
           cds/1,
           utr/1,
           splice_site/1,
           three_prime_cis_splice_site/1,
           five_prime_cis_splice_site/1,
           polypeptide/1,
           sequence_variant/1,
           transgene/1,
	   regulatory_region/1,
	   
           start_codon_exon_pos/3,
           stop_codon_exon_pos/3,
           intron_dnaseq_pos/5,
           feature_dnaseq_pos/5,
           exon_dnaseq_pos/5,
           cds_dnaseq_pos/5,
           exon_transcript/2,
           exon_transcript_order/3,
           exon_intron_exon_transcript/4,
           intron_transcript_order/3,
           gene_length/2,
           gene_overlaps/2,
           gene_left_of/2,
           exon_gene/2,
           intron_gene/2,
           intron_transcript/2,
           gene_polypeptide/2,
           gene_exon_count/2,
           single_exon_gene/1,

	   gene_symbol/2
          ]).

:- use_module(bio(range)).
:- use_module(bio(bioseq)).
:- use_module(bio(metadata_db)).
:- use_module(bio(ontol_db)).
:- use_module(bio(bioprolog_util)).

% TODO: split into general types and specific types?

% metadata on the data predicates used in this module
:- use_module(bio(dbmeta)).

% ----------------------------------------
% instance predicates
% ----------------------------------------
:- extensional(gene/1).
:- extensional(transcript/1).
:- extensional(cds/1).
:- extensional(polypeptide/1).
:- extensional(exon/1).
:- extensional(intron/1).
:- extensional(dnaseq_name/2).
:- extensional(sequence_variant/1).
:- extensional(regulatory_region/1).
:- extensional(transposable_element/1).
:- extensional(psuedogene/1).
:- extensional(utr/1).
:- extensional(three_prime_utr/1).
:- extensional(five_prime_utr/1).
:- extensional(codon/1).
:- extensional(start_codon/1).
:- extensional(stop_codon/1).
:- extensional(transgene/1).

:- extensional(splice_site/1).
:- extensional(three_prime_cis_splice_site/1).
:- extensional(five_prime_cis_splice_site/1).

:- extensional(single_exon_gene/1).

% ----------------------------------------
% relations
% ----------------------------------------
%% gene_type(?Gene,?Type)
% reified relationship: Type name of a subtype of gene from SO
:- extensional(gene_type/2).

:- extensional(regulates_gene/2).
:- extensional(regulates_transcript/2).

:- extensional(gene_transcript/2).
:- extensional(transcript_cds/2).
:- extensional(exon_transcript/2).
:- extensional(exon_transcript_order/3).
:- extensional(intron_transcript/2).
:- extensional(intron_transcript_order/3).

% ----------------------------------------
% locative predicates
% ----------------------------------------

% TODO: use more generic versions..

:- extensional(gene_dnaseq_pos/5).
:- extensional(transcript_dnaseq_pos/5).
:- extensional(transcript_gene_pos/4).
:- extensional(exon_dnaseq_pos/5). 
:- extensional(cds_exon_pos_pair/5).
:- extensional(cds_dnaseq_pos/5).
:- extensional(exon_primary_transcript_pos/4).
:- extensional(exon_intron_exon_transcript/4).
:- extensional(intron_dnaseq_pos/5).
:- extensional(start_codon_exon_pos/3).
:- extensional(stop_codon_exon_pos/3).


% ----------------------------------------
% metadata
% ----------------------------------------

:- extensional(gene_symbol/2).

% ----------------------------------------
% intensional predicates
% ----------------------------------------

% MOVED TO GRULE FILES

% TODO: sql_compiler: translate 
%feature_dnaseq_pos(F,Seq,Beg,End,Str) :- intron_dnaseq_pos(F,Seq,Beg,End,Str).
feature_dnaseq_pos(F,Seq,Beg,End,Str) :- gene_dnaseq_pos(F,Seq,Beg,End,Str).
feature_dnaseq_pos(F,Seq,Beg,End,Str) :- exon_dnaseq_pos(F,Seq,Beg,End,Str).
feature_dnaseq_pos(F,Seq,Beg,End,Str) :- transcript_dnaseq_pos(F,Seq,Beg,End,Str).


%% exon_gene(?Exon,?Gene)
% composition of exon_transcript_order/3 . gene_transcript/2
exon_gene(X,G):-
        exon_transcript_order(X,T,_),
        gene_transcript(G,T).

%% gene_polypeptide(G,P)
% TODO: mixing pp and cds for now...
gene_polypeptide(G,P) :-
        gene_transcript(G,T),
        transcript_cds(T,P).

intron_transcript(I,T):-
        intron_transcript_order(I,T,_).

intron_gene(I,G):-
        intron_transcript(I,T),
        gene_transcript(G,T).


gene_exon_count(G,XC):-
        XC is count(X,exon_gene(X,G)).

/*
dnapos_to_spliced_rna(Pos,T,PosT) :-
        exon_transcript_order(X,T,R),
        exon_dnaseq_pos(X,Seq,Beg,End,Str),
        junction_in_range(Pos,Beg,End,Str), % TODO
*/        

%% gene_length(?G,?Len)
% length of gene on genome
gene_length(G,Len):-
        gene_dnaseq_pos(G,_,Beg,End,_),
        Len is (End-Beg).

%% gene_overlaps(G1,G2)
% true if the genomic extent of G1 and G2 share at least one base.
% Reflexive.
gene_overlaps(G1,G2):-
        gene_dnaseq_pos(G1,Seq,Beg1,End1,Str),
        gene_dnaseq_pos(G2,Seq,Beg2,End2,Str),
        End1 >= Beg2,
        Beg1 =< End2.

%% gene_upstream_of(G1,G2)
% true if the genomic extent of G1 is upstream of G2 
gene_upstream_of(G1,G2):-
        gene_dnaseq_pos(G1,Seq,Beg1,End1,Str),
        gene_dnaseq_pos(G2,Seq,Beg2,End2,Str),
        (   Str>0
        ->  End1 =< Beg2
        ;   Beg1 >= End2).
        

%% gene_left_of(G1,G2)
% true if the genomic extent of G1 is < G2 
gene_left_of(G1,G2):-
        gene_dnaseq_pos(G1,Seq,_,End1,Str),
        gene_dnaseq_pos(G2,Seq,Beg2,_,Str),
        End1 =< Beg2.


:- multifile dbmeta:fact_chain_hook/2.
dbmeta:fact_chain_hook(gene(G),
		       [gene_symbol(G,_),
			metadata_db:entity_xref(G,_),
			metadata_db:entity_label(G,_)]).


/** <module> Represents genomic features and their relationships

  ---+ Synopsis

==
:- use_module(bio(genome_db)).

demo:-
    rdb_connect(H,homo_sapiens_core),
    rdb_forall(H,
               (  gene_symbol(G,'BRCA1'),gene_transcript(G,T),genome_db:exon_transcript_order(X,T,Rank)),
               format('BCRCA1: Gene:~w Tr:~w Exon:~w at w~n',[G,T,X,Rank])).
==

  ---+ Package

  This module is part of the blipkit genomic package. See
  README.txt

---+ Details

The genome_db module consists of a direct representation of features and their relations within prolog. For example, there are predicates such as transcript/1, exon/1, regulates_gene/2.

The schema is similar to ensembl. This module works well in combination with an Ensembl MySQL database. See genome_sqlmap_enscore.pro

This module presents an alternative representation to seqfeature_db.pro, in which feature types and relationships are reified.

@author  Chris Mungall
@version $Revision$
@see     README
@license License


*/
