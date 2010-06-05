:- use_module(bio(genome_db)).
:- use_module(bio(ontol_db)).
:- use_module(bio(seqanalysis_db)).
:- use_module(bio(bioseq)).
:- use_module(bio(range)).
:- use_module(bio(gencode)).
:- use_module(bio(hmm)).
:- use_module(bio(gff)).
:- use_module(bio(iupac)).
:- use_module(bio(parser_fasta)).

:- use_module(bio(genome_sqlmap_enscore)).
:- use_module(bio(seqfeature_sqlmap_chado)).
:- use_module(bio(seqfeature_sqlmap_chado_exposed_ids)).
:- use_module(bio(seqfeature_sqlmap_go)).
:- [bio('sql_schema/schema_enscore44')].
:- [bio('sql_schema/chado')].

:- use_module(bio(seqfeature_owlmap_node_as_class)).
:- use_module(bio(seqfeature_owlmap_node_as_individual)).

:- use_module(bio(seqfeature_writer_chaos)).

:- use_module(bio(seqfeature_xmlmap_chado)).
:- use_module(bio(seqfeature_xmlmap_chaos)).
:- use_module(bio(seqfeature_xmlmap_game)).
