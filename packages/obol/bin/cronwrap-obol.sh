#!/bin/bash
. $HOME/.profile
OBOL='obol -r go_synonyms  -r ubo -r relationship -r spatial -r go -table_pred process5/3 -table_pred process/3 -table_pred any_kind_of/3 -table_pred continuant/3 -table_pred subclassT/2 -u obol_go_xp_all -r pato -r cell -r xchebi'
GO_XP_CHEBI=$HOME/cvs/obo/ontology/cross_products/go_chebi_xp/GO_to_ChEBI.obo
cd $HOME/cvs/obo && cvs update
cd $HOME/cvs/go/ontology && cvs update
cd $HOME/cvs/go/scratch/obol_results && $OBOL obol-parse -parse_rule process -i ro_test.obo -i ../gene_ontology_xp.obo -xp_policy newonly -i $GO_XP_CHEBI "belongs(ID,biological_process)" >& biological_process_xp-obol.obo
cd $HOME/cvs/go/scratch/obol_results && $OBOL obol-parse -parse_rule cellular_component -i ro_test.obo -xp_policy newonly "belongs(ID,cellular_component_process)" >& cellular_component_xp-obol.obo
