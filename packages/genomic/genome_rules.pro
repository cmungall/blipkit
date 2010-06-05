% DEPRECATED

type(gene).
type(transcript).
type(cds).
type(polypeptide).
type(exon).
type(intron).
type(variant).
type(utr).
type(regulatory_region).

encodes holds_between gene * transcript.
transcript_cds holds_between transcript * cds.
exon_transcript_order holds_between exon * transcript * datatype(int).
first_exon_on_transcript(X,T) <- exon_transcript_order(X,T,0).
consecutive_exons_on_transcript(X1,X2,T) <- exon_transcript_order(X1,T,Ord1),exon_transcript_order(X2,T,Ord2),Ord2=Ord1+1.

transcript_spliced_seq(T,S,0) <- exon_on_transcript(X,T,0),exon_seq(X,S).
transcript_spliced_seq(T,S,Ord) <- Ord>0, exon_on_transcript(X,T,Ord),transcript_spliced_seq(T,S,OrdP),OrdP=Ord-1.

:- extensional(variant/1).
:- extensional(regulatory_region/1).

:- extensional(gene_transcript/2).
:- extensional(transcript_cds/2).
:- extensional(exon_transcript_order/3).

% normalize these further?
:- extensional(gene_dnaseq_pos/5).
:- extensional(transcript_dnaseq_pos/5).
:- extensional(transcript_gene_pos/4).
:- extensional(exon_dnaseq_pos/5).
:- extensional(exon_primary_transcript_pos/4).

:- extensional(gene_symbol/2).
