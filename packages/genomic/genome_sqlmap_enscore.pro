/* -*- Mode: Prolog -*- */


:- module(genome_sqlmap_enscore,
          [
          ]).

:- use_module(bio(sql_compiler)).
:- use_module(bio(rdb_util)).
:- use_module(bio(ontol_db),[]).
:- use_module(bio(metadata_db),[]).
:- use_module(bio(genome_db),[]).
:- [bio('grule/gruleset_enscore')]. % rules for introns etc

:- load_schema_defs(bio('sql_schema/schema_enscore51')).

:- multifile
	system:term_expansion/2.

gene(GI,G,A,SRI,Start,End,Strand) <-
  gene(GI,G,A,SRI,Start,End,Strand,_).


gene_s(InternalID,PublicID,Symbol) <-
  gene(InternalID,_,_,_,_,_,_,X),gene_stable_id(InternalID,PublicID),xref(X,_,_,Symbol).

gene_i(InternalID,PublicID) <-
  gene(InternalID,_,_,_,_,_,_,_),gene_stable_id(InternalID,PublicID).

transcript_i(TI,T) <-
  transcript(TI,_,_,_,_,_,_,_),transcript_stable_id(TI,T).

translation_i(PI,P) <-
  translation(PI,_,_,_,_,_),translation_stable_id(PI,P).

genome_db:gene(G) <-
  gene_i(_,G).

gene_xref(G,X) <-
  gene_i(GI,G),object_xref(_,GI,'Gene',XI,_),xref(XI,_,_,X,_,_,_,_).

metadata_db:entity_synonym(G,S) <-
  gene_i(GI,G),object_xref(_,GI,'Gene',XI,_),external_synonym(XI,S).

seq_region0(S,SN) <-
  seq_region(S,SN,_,_).

genome_db:seq_subseq(Seq,Start,End,SubSeq) <-
  seq_region0(SRI,Seq),
  dna(SRI,DNA),
  eval(SubSeq, substr(DNA,Start,End-Start)).
%  SubSeq is substr(Seq,Start,End).


genome_db:regulatory_region_dnaseq_pos(R,Seq,Start,End,Strand) <-
  %regulatory_feature(RI,R),
  regulatory_feature(_RI,R,SRI,Start,End,Strand,_,_FacI),
  seq_region0(SRI,Seq).

% NOT USED - now in funcgen
%genome_db:xxxregulates_transcript(R,T) <-
%  regulatory_feature(_RI,R,_DNASeqI,_Start,_End,_Strand,_,FacI),
%  regulatory_factor_coding(FacI,TI,_),
% transcript_i(TI,T).

% sql_compiler does this for us but does not optimize it..
regulatory_feature(RI,R) <-
  regulatory_feature(RI,R,_,_,_,_,_,_).


genome_db:regulates_gene(R,G) <-
  %regulatory_feature(RI,R,_,_,_,_,_,_),
  regulatory_feature(RI,R),
  regulatory_feature_object(RI,'Gene',GI),
  gene_i(GI,G).

genome_db:regulates_transcript(R,G) <-
  regulatory_feature(RI,R),
  regulatory_feature_object(RI,'Transcript',GI),
  gene_i(GI,G).

genome_db:gene_symbol(G,S) <-
  gene(InternalID,_,_,_,_,_,_,X),gene_stable_id(InternalID,G),xref(X,_,_,S).

genome_db:exon(X) <-
    exon(X,_,_,_,_,_,_,1).

% POSITIONS
% TODO: translate (min,max) to IB (s,e) in separate layer?

%genome_db:gene_dnaseq_pos(G,Seq,Start,End,Strand) <-
%  gene_i(GI,G),
%  gene(GI,_,_,SRI,Start,End,Strand),
%  seq_region0(SRI,Seq).
genome_db:gene_dnaseq_pos(G,Seq,Start,End,Strand) <-
  gene_i(GI,G),
  gene(GI,_,_,SRI,Start,End,Strand),
  Strand= 1,
  seq_region0(SRI,Seq).
genome_db:gene_dnaseq_pos(G,Seq,Start,End,Strand) <-
  gene_i(GI,G),
  gene(GI,_,_,SRI,End,Start,Strand),
  Strand= -1,
  seq_region0(SRI,Seq).

genome_db:transcript_dnaseq_pos(T,Seq,Start,End,Strand) <-
  transcript_i(TI,T),
  transcript(TI,_,_,SRI,Start,End,Strand),
  seq_region0(SRI,Seq).

%genome_db:exon_dnaseq_pos(X,Seq,Start,End,Strand) <-
%  exon(X,SRI,Start,End,Strand,_,_,1),
%  seq_region0(SRI,Seq).
genome_db:exon_dnaseq_pos(X,Seq,Start,End,Strand) <-
  exon(X,SRI,Start,End,Strand,_,_,1),
  Strand= 1,
  seq_region0(SRI,Seq).
genome_db:exon_dnaseq_pos(X,Seq,Start,End,Strand) <-
  exon(X,SRI,End,Start,Strand,_,_,1),
  Strand= -1,
  seq_region0(SRI,Seq).


%genome_db:dnaseq_name(S,N) <-
%  seq_region(S,N,_,_).

genome_db:dnaseq_length(S,Len) <-
  seq_region(S,_,_,Len).


genome_db:transcript(T) <-
  transcript_i(_,T).

genome_db:exon_transcript_order(X,T,Rank) <-
  transcript_i(TI,T),
  exon_transcript(X,TI,Rank).

genome_db:gene_transcript(G,T) <-
  transcript(TI,GI,_,_,_,_,_,_),
  transcript_i(TI,T),
  gene_i(GI,G).

genome_db:transcript_cds(T,P) <-
  translation(PI,TI,_,_,_,_),
  transcript_i(TI,T),
  translation_i(PI,P).

genome_db:cds_exon_pos_pair(P,X1,Beg,X2,End) <-
  translation(PI,_,Beg,X1,End,X2),
  translation_i(PI,P).

sql_compiler:view(gene_type(G,T),
                  (   gene(InternalID,BioType,_,_,_,_,_,_),
                      gene_stable_id(InternalID,G))):-
        genetype_ftype(BioType,T).
xxgenome_db:gene_type(G,T) <-
  gene(InternalID,BioType,_,_,_,_,_,_),
  gene_stable_id(InternalID,G)
  ::
  genome_sqlmap_enscore:genetype_ftype(BioType,T).

genome_db:gene_biotype(G,BioType) <-
  gene(InternalID,BioType,_,_,_,_,_,_),
  gene_stable_id(InternalID,G).

genome_db:mitochondrial(G) <-
  gene_dnaseq_pos(G,'MT',_,_,_).

genetype_ftype('V_segment'). 
genetype_ftype('J_segment'). 
genetype_ftype('C_segment'). 
genetype_ftype('D_segment'). 
genetype_ftype(protein_coding,protein_coding_gene). 
genetype_ftype(pseudogene,pseudogene). % mismatch
genetype_ftype(retrotransposed,retrogene). % check
genetype_ftype(repeat,repeat_gene).        % check
genetype_ftype('Mt_tRNA',tRNA_gene).       % 
genetype_ftype('Mt_rRNA',rRNA_gene). 
genetype_ftype(misc_RNA,ncRNA_gene). 
genetype_ftype(snRNA,snRNA_gene). 
genetype_ftype(miRNA,miRNA_gene). 
genetype_ftype(snoRNA,snoRNA_gene). 
genetype_ftype(rRNA,rRNA_gene). 
genetype_ftype(scRNA_pseudogene,scRNA_pseudogene). 
genetype_ftype(snoRNA_pseudogene,snoRNA_pseudogene). 
genetype_ftype(snRNA_pseudogene,snRNA_pseudogene). 
genetype_ftype(rRNA_pseudogene,rRNA_pseudogene). 
genetype_ftype(tRNA_pseudogene,tRNA_pseudogene). 
genetype_ftype(misc_RNA_pseudogene,misc_RNA_pseudogene). 
genetype_ftype(scRNA,scRNA_gene). 
genetype_ftype('Mt_tRNA_pseudogene',tRNA_pseudoene). 
genetype_ftype(miRNA_pseudogene,miRNA_pseudogene). 


:- dynamic rdb_handle/1.
getrdb(Rdb):-
        nb_getval(rdb,Rdb).
setrdb(Rdb):-
        nb_setval(rdb,Rdb).



/** <module> Mapping between genome_db and Ensembl Core

  ---+ Synopsis

  ==
  :- use_module(bio(genome_sqlmap_enscore)).

  demo:-
    rdb_connect(H,homo_sapiens_core),
    rdb_forall(H,
              (  gene_symbol(G,'BRCA1'),gene_transcript(G,T),genome_db:exon_transcript_order(X,T,Rank)),
             format('BCRCA1: Gene:~w Tr:~w Exon:~w at w~n',[G,T,X,Rank])).
  ==

  Uses gene_symbol/2, gene_transcript/2 and exon_transcript_order/3

  command line:

  ==
  # find all regulatory regions for BRCA1
  blip-sql -r enscore/homo_sapiens_core sql-map -u genome_sqlmap_enscore -proj G,R "(gene_symbol(G,'BRCA1'),regulates_gene(R,G))"
  ==

  Uses gene_symbol/2, regulates_gene/2

  
  ---+ Description

  This allows a SQL Database using the Enscore Schema to masquerade as genome_db.pro
  predicates

  ---+ More examples

  See the plunit tests in genome_sqlmap_enscore.plt

  ==
  # position of BRCA1
  blip-sql -r enscore/homo_sapiens_core sql-map -u genome_sqlmap_enscore "(gene_symbol(G,'BRCA1'),gene_dnaseq_pos(G,Seq,Beg,End,Str))"
  ==
  
  ==
  # find all regulatory regions for BRCA1, and their positions
  blip-sql -r enscore/homo_sapiens_core sql-map -u genome_sqlmap_enscore "(gene_symbol(G,'BRCA1'),regulates_gene(R,G),regulatory_region_dnaseq_pos(R,Seq,Beg,End,Str))"
  ==
  
  ==
  # find all BRCA1 introns
  blip-sql -r enscore/homo_sapiens_core sql-map -u genome_sqlmap_enscore "(gene_symbol(G,'BRCA1'),intron_gene(I,G),intron_dnaseq_pos(I,Seq,Beg,End,Str))" -project Beg-End
  ==

  ==
  # Length of longest gene (two steps, plug the result of the first into the second)
  blip-sql -r enscore/homo_sapiens_core sql-map -u genome_sqlmap_enscore -proj ML "ML is max(Len,(gene_dnaseq_pos(G,Seq,Beg,End,Str),Len is End-Beg))"
  blip-sql -r enscore/homo_sapiens_core sql-map -u genome_sqlmap_enscore -proj G,GS "(gene_symbol(G,GS),gene_dnaseq_pos(G,Seq,Beg,End,Str),2304117 is End-Beg)"
  ==
  
  ==
  # Length of longest gene (one step)
  blip-sql -r enscore/homo_sapiens_core sql-map -u genome_sqlmap_enscore -proj G-GS-ML "(ML is max(Len,(gene_dnaseq_pos(_,_,Beg,End,_),Len is End-Beg)),gene_symbol(G,GS),gene_dnaseq_pos(G,Seq,Beg1,End1,Str),ML is End1-Beg1)"
  ==
  
  If you run this last one with =|-debug sql|= you will see the SQL is quite voluminous. Note the prolog can be made even smaller via a gene_length/2 predicate:

  ==
  blip-sql -u genome_db -r enscore/homo_sapiens_core sql-map -u genome_sqlmap_enscore -proj G-GS-ML "(ML is max(Len,(gene_length(_,Len))),gene_symbol(G,GS),gene_length(G,ML))"
  ==

  Note that gene_length/2 can be used over a normal prolog database, or a SQL database, it doesn't matter (although this is not true for all predicates - recursive predicates cannot be translated to SQL)

  Find all overlapping genes: WARNING! SLOW! MySQL has poor optimization for ranges

  ==
  blip-sql -u genome_db -debug sql -r enscore/homo_sapiens_core sql-map -u genome_sqlmap_enscore "gene_overlaps(G,G2)"
  ==

  Uses gene_overlaps/2

  ==
  blip-sql -u genome_db -debug sql -u genome_sqlmap_enscore prolog-to-sql "single_exon_gene(G)"
  ==

  Uses single_exon_gene/1
  
  ---+ TODO

  Enscore uses base-oriented. Should do conversion to interbase here

  ---+ Debug Codes

  * sql
  * sql_compiler
  
  ---+ See Also

  @author Chris Mungall
  @version  $Revision: 1.4 $
  @see schema_enscore44.pro, ../sql/odbc_setup.txt, sql_compiler.pro, plterm_to_sqlterm/3
  @license LGPL

  */
