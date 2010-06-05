% Terminating subset of rules geared towards ensembl core model
% @see genome_sqlmap_enscore

:- use_module(bio(genome_db)).
:- use_module(bio(genome_bridge_from_seqfeature)).

:- [intron].
:- [intron_dnaseq_pos].
%:- [feature_subtypes_dnaseq_pos].
%:- [codons_from_cds].
:- [single_exon_gene].
:- [exon_transcript_order].
:- [splice_site_from_exon].


