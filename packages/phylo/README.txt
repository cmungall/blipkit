---+ phylo - Phylogenetic Models

The core model is phylo_db.pro -- this module contains predicates for
representing and querying phylogenetic trees. Example predicates:

 * phylonode/1
 * phylonode_parent/3

The module homol_db.pro is for representing and querying sets of
homologous genes or gene products. The model here is deliberately
simplistic, and follows homology-set resources such as
HomoloGene. Example predicates:

  * homologset_member/2
  * homologous_to/2

The module taxon_db.pro for representing simple taxonomies is
deprecated. Using ontol_db.pro instead (e.g. use subclass/2 and
subclassT/2 for taxonomic hierarchy queries). The NCBI taxonomy can be
accessed as an ontology like this:

==
blip -r obo/ncbi_taxonomy
==

---++ Mappings and Parsers

* homoltbl_bridge_to_homol.pro -- maps tab-delimited HomoloGene facts to homol_db.pro predicates

---+++ Parsers

* parser_nhx.pro -- New Hampshire (NH and NHX)

---+++ XML Bindings

* phylo_xmlmap_phyloxml.pro -- Phylo-XML
* phylo_writer_chadoxml.pro -- ChadoXML
* homol_xmlmap_homologene.pro -- HomoloGene XML translation to homol_db model

---+++ Database Mappings

* phylo_sqlmap_chado.pro -- maps from phylo module in a Chado database
* phylo_sqlmap_enscompara.pro -- access of phylo trees in an enscompara database
* homol_sqlmap_go.pro -- maps to the homology tables in the GO databases
* homol_sqlmap_enscompara.pro -- access of simple homology pairings in enscompara database

---+++ OWL and Ontology Model Mappings

* phylo_bridge_to_ontol.pro




