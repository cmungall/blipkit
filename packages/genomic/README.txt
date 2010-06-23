---+ genomic - Genome sequence and architecture


There are currently two complementary core models for representing genomic features:

 * seqfeature_db.pro
 * genome_db.pro

In addition, there are a few model and utility modules for
manipulating feature data that are independent of the core model used

 * bioseq.pro
 * range.pro
 * seqanalysis_db.pro -- analyzing sequence alignments

---++ Models

---+++ genome_db

The genome_db.pro module consists of a direct representation of features
and their relations within prolog. For example, there are predicates
such as transcript/1, exon/1, regulates_gene/2.

---+++ seqfeature_db

In contrast, the seqfeature_db.pro module partially *reifies* feature
types and relations, placing them in the domain of discourse. Genomic
features can be queried using generic predicates such as feature/1,
feature_relationship/4 and featureloc/8. Type columns can then be
queried via ontol_db predicates such as subclass/2 and class/2. For
example, the following goal retrieves all exons in the
database:

==
class(X,exon),feature_type(F,X)
==

However, this only fetches features directly typed to *exon*. The
following goal illustrates unification with all features that are
transcripts or some subtype of transcript:

==
class(T,trancript),subclassRT(TC,T),feature_type(F,TC)
==

This assumes that the sequence ontology has been loaded into the
session database, e.g. via load_bioresource/1

==
load_bioresource(obo(sequence)),class(T,trancript),subclassRT(TC,T).
==

---++++ Comparison between seqfeature_db and genome_db

At this time the seqfeature_db is more mature. Use of the ontol_db
module make inference more user controlled.

Bridge Modules:

 * genome_bridge_from_seqfeature.pro
 * genome_bridge_to_seqfeature.pro

---+++ seqanalysis_db

Represents the results of running pairwise sequnce alignments. Also
used for transforming coordinate systems.

See for example:

 * hsp/14
 * hit/5
 * parse_cigar/2

In future there will be a bridge module to go back and forth between
blast-centric hsp/14 and hit/5 facts and the more generic
seqfeature_db facts

---+++ fasta_db

Represents simple ID-Description-Sequence triples using fastaseq/3 facts

Bridge module: seqfeature_bridge_from_fasta.pro

---++ Mappings and Parsers

---+++ XML Bindings

 * seqfeature_xmlmap_chado.pro -- ChadoXML
 * seqfeature_xmlmap_chaos.pro -- ChaosXML
 * seqfeature_xmlmap_game.pro -- GameXML

---+++ Database Mappings

 * seqfeature_sqlmap_chado.pro -- ChadoSQL
 * genome_sqlmap_enscore.pro  -- Ensembl Core

---+++ OWL and Ontology Model Mappings

 * seqfeature_owlmap_node_as_class.pro

---+++ Parsers

 * fasta

---++ Module dependencies

 * ontol_db.pro for feature types

---++ Tutorials and further documentation

* doc/exploring_genomic_data.txt
* doc/genomic_cookbook.txt








