---+ ontol - Ontology Package

---++ Abstract

The ontol_db.pro module models OBO-style ontologies

---++ Core Modules

The core module is the ontol_db model

  * [[ontol_db][ontol_db.pro]]

  * ontol_segmenter.pro -- for extracting subsets of ontologies
  * ontol_management.pro -- class merging and ontology lifecycle
  * ontol_reasoner.pro -- forward chaining reasoning

---++ Package dependencies

This package is part of blip. See ../../README.txt for details

  * [[metadata][../metadata/README.txt]]

The metadata package (metadata_db.pro) is used 

---++ Tutorials

* [[Ontol package tutorials][<doc/>]]

---++ Details

---++ Parsers and Writers

---++ Mappings

---+++ Database Mappings

  * ontol_sqlmap_go.pro

---+++ OWL Mappings

  * ontol_bridge_from_owl.pro
  * ontol_bridge_to_owl.pro

---++ See Also

Thea2 OWL package
