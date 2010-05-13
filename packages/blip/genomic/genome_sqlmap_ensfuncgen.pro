/* -*- Mode: Prolog -*- */


:- module(genome_sqlmap_ensfuncgen,
          [
          ]).

:- use_module(bio(sql_compiler)).
:- use_module(bio(rdb_util)).
:- use_module(bio(ontol_db),[]).
:- use_module(bio(metadata_db),[]).
:- use_module(bio(genome_db),[]).

:- use_module(genome_sqlmap_enscore).

:- load_schema_defs(bio('sql_schema/schema_ensfuncgen51')).

:- multifile
	system:term_expansion/2.

gene_i(InternalID,PublicID) <-
  gene(InternalID,_,_,_,_,_,_,_),gene_stable_id(InternalID,PublicID).

%regulatory_feature_i(RI,R) <-
%  regulatory_feature(RI,_DNASeqI,_Start,_End,_Strand,_Disp,_Type,_Set,StableID),


genome_db:regulatory_region_dnaseq_pos(R,Seq,Start,End,Strand) <-
  regulatory_feature(R,DNASeqI,Start,End,Strand,_Disp,_Type,_Set,_Stable),
  seq_region0(DNASeqI,Seq).

%genome_db:regulates_transcript(R,T) <-
%  regulatory_feature(R,_DNASeqI,_Start,_End,_Strand,_Disp,_Type,_Set,_Stable),
%  regulatory_factor_coding(FacI,TI,_),
%  transcript_i(TI,T).
