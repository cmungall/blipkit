# add the following line to your .profile
# . $HOME/cvs/blipkit/etc/blipkit-profile.sh
# (adjusting to wherever your blipkit is located)

# --Paths--
export PATH="$HOME/cvs/blipkit/bin:$PATH"
# --Paths for blipkit apps--
export PATH="$HOME/cvs/bioprolog/apps/amigo/src:$PATH"
export PATH="$HOME/cvs/bioprolog/apps/amipath/src:$PATH"
export PATH="$HOME/cvs/bioprolog/apps/nlp/bin:$PATH"
export PATH="$HOME/cvs/blipkit/packages/obol/bin:$PATH"
export PATH="$HOME/cvs/bioprolog/apps/jblip/bin:$PATH"

# --see mode.pro for an explanation--
alias plmodeon="export PROLOG_MODE_CHECKING=1"
alias plmodeoff="export PROLOG_MODE_CHECKING=0"

# --convenience aliases for querying ontologies--
alias tq="blip -r taxnames -r taxnodes -u phylo_bridge_from_taxon"
alias oq="blip ontol-query"
alias ov="blip ontol-subset"
alias go="ov -r go"
alias qgo="oq -r go"
alias taxo="ov -r taxonomy"
alias qtaxo="oq -r taxonomy"
alias fly="ov -r fly_anatomy"
alias qfly="oq -r fly_anatomy"
alias flydev="ov -r fly_development"
alias qflydev="oq -r fly_development"
alias bila="ov -r bila"
alias qbila="oq -r bila"
alias aba="ov -r aba"
alias qaba="oq -r aba"
alias bmap="ov -r bm"
alias qbmap="oq -r bm"
alias zf="ov -r zebrafish_anatomy"
alias qzf="oq -r zebrafish_anatomy"
alias tao="ov -r teleost_anatomy"
alias qtao="oq -r teleost_anatomy"
alias hao="ov -r hao"
alias qhao="oq -r hao"
alias tto="ov -r teleost_taxonomy"
alias qtto="oq -r teleost_taxonomy"
alias spdo="ov -r spider_anatomy"
alias qspdp="oq -r spider_anatomy"
alias mosq="ov -r mosquito_anatomy"
alias qmosq="oq -r mosquito_anatomy"
alias tick="ov -r tick_anatomy"
alias qtick="oq -r tick_anatomy"
alias qpato2="oq -r pato2"
alias pato2="ov -r pato2"
alias pato="ov -r pato"
alias qpato="oq -r pato"
alias evoc="ov -r obo/evoc"
alias qevoc="oq -r obo/evoc"
alias miro="ov -r miro"
alias qmiro="oq -r miro"
alias mesh="ov -r mesh"
alias qmesh="oq -r mesh"
alias hog="ov -r hog"
alias qhog="oq -r hog"
alias vhog="ov -r vhog"
alias qvhog="oq -r vhog"
alias so="ov -r so"
alias qso="oq -r so"
alias so2="ov -r so2"
alias qso2="oq -r so2"
alias cell="ov -r cell"
alias qcell="oq -r cell"
alias chebi="ov -r chebi"
alias qchebi="oq -r chebi"
alias chebicb="ov -r chebi -rel subclass -rel is_conjugate_base_of"
alias qchebicb="oq -r chebi -rel subclass -rel is_conjugate_base_of"
alias goche="ov -r goche"
alias qgoche="oq -r goche"
alias plant="ov -r plant"
alias qplant="oq -r plant"
alias plantdev="ov -r plant_development"
alias qplantdev="oq -r plant_development"
alias mouse="ov -r mouse_anatomy"
alias qmouse="oq -r mouse_anatomy"
alias worm="ov -r worm_anatomy"
alias qworm="oq -r worm_anatomy"
alias wormls="ov -r worm_development"
alias qwormls="oq -r worm_development"
alias emap="ov -r emap"
alias qemap="oq -r emap"
alias emapa="ov -r emapa"
alias qemapa="oq -r emapa"
alias emapaa="ov -r emapaa"
alias qemapaa="oq -r emapaa"
alias ehdaa2="ov -r ehdaa2"
alias qehdaa2="oq -r ehdaa2"
alias wn="ov -r ontol_wn"
alias qwn="oq -r ontol_wn"
alias disease="ov -r disease"
alias qdisease="oq -r disease"
alias mpath="ov -r mpath"
alias qmpath="oq -r mpath"
alias fma="ov -r fma"
alias qfma="oq -r fma"
alias sfma="ov -r fma_simple"
alias qsfma="oq -r fma_simple"
alias fma3="ov -r fma3"
alias qfma3="oq -r fma3"
alias fma3h="ov -r fma3h"
alias qfma3h="oq -r fma3h"
alias mp="ov -r mammalian_phenotype"
alias qmp="oq -r mammalian_phenotype"
alias hp="ov -r human_phenotype"
alias qhp="oq -r human_phenotype"
alias ascp="ov -r ascomycete_phenotype"
alias qascp="oq -r ascomycete_phenotype"
alias wp="ov -r worm_phenotype"
alias qwp="oq -r worm_phenotype"
alias pt="ov -r plant_trait"
alias qpt="oq -r plant_trait"
alias ncit="ov -r ncit"
alias qncit="oq -r ncit"
alias galen="ov -r galen"
alias qgalen="oq -r galen"
alias caro="ov -r caro"
alias qcaro="oq -r caro"
alias aeo="ov -r aeo"
alias qaeo="oq -r aeo"
alias tick="ov -r tick_anatomy"
alias qtick="oq -r tick_anatomy"
alias frog="ov -r xenopus_anatomy"
alias qfrog="oq -r xenopus_anatomy"
alias aao="ov -r amphibian_anatomy"
alias qaao="oq -r amphibian_anatomy"
alias dicty="ov -r dicty_anatomy"
alias qdicty="oq -r dicty_anatomy"
alias fungal="ov -r fungal_anatomy"
alias qfungal="oq -r fungal_anatomy"
alias bfo="ov -r bfo"
alias qbfo="oq -r bfo"
alias protein="ov -r protein -r pro2uniprot"
alias qprotein="oq -r protein -r pro2uniprot"
alias envo="ov -r envo"
alias qenvo="oq -r envo"
alias gaz="ov -r gaz"
alias qgaz="oq -r gaz"
alias obi="ov -r obi"
alias qobi="oq -r obi"
alias uber="ov -r uberon"
alias quber="oq -r uberon"
alias uberp="ov -r uberonp"
alias quberp="oq -r uberonp"
alias upheno="ov -r upheno"
alias qupheno="oq -r upheno"
alias evcode="ov -r evidence_code"
alias qevcode="oq -r evidence_code"
alias omim="ov -r omim"
alias qomim="oq -r omim"
alias nifa="ov -r birnlex_anatomy"
alias qnifa="oq -r birnlex_anatomy"
alias birn="ov -r nif_downcase"
alias qbirn="oq -r nif_downcase"
alias ido="ov -r ido"
alias qido="oq -r ido"
alias brenda="ov -r brenda"
alias qbrenda="oq -r brenda"
alias vao="ov -r vao"
alias qvao="oq -r vao"

alias sao="ov -r sao -u ontol_manifest_metadata_from_sao"
alias qsao="oq -r sao -u ontol_manifest_metadata_from_sao"

alias hgene="ov -r gene/9606"
alias qhgene="oq -r gene/9606"
alias mgi="ov -r gene/10090"
alias qmgi="oq -r gene/10090"

alias mpx="ov -r mammalian_phenotype -r mammalian_phenotype_xp -r mammalian_phenotype_xp_nif -r mammalian_phenotype_xp_uberon -r mouse_anatomy -r go -r pato -showxp"
alias qmpx="oq -r mammalian_phenotype -r mammalian_phenotype_xp -r mouse_anatomy -r go -r pato -showxp"
alias hpx="ov -r human_phenotype -r human_phenotype_xp -r fma2 -r go -r pato -showxp"
alias qhpx="oq -r human_phenotype -r human_phenotype_xp -r fma2 -r go -r pato -showxp"
alias ptx="ov -r plant_trait -r plant_trait_xp -r plant_anatomy -r go -r pato -showxp"
alias qptx="oq -r plant_trait -r plant_trait_xp -r plant_anatomy -r go -r pato -showxp"
alias diseasex='ov -r disease_xp -showxp'
alias qdiseasex='oq -r disease_xp -showxp'
PHENO_ARGS='-r mammalian_phenotype -r human_phenotype'
alias pheno='ov $PHENO_ARGS'
alias qpheno='oq $PHENO_ARGS'

export MULTIANAT_R='-r xenopus_anatomy -r mosquito_anatomy -r tick_anatomy -r spider_anatomy -r hao -r mouse_anatomy -r gemina_anatomy -r amphibian_anatomy -r cell -r fly_anatomy -r zebrafish_anatomy -r fma_downcase -r brenda -r bila -r miaa -r nif_downcase -r emapaa -r ehdaa -r ehdaa2 -r hog -r wpanat -r mesh -r obo/evoc'
alias multianat="blip ontol-query $MULTIANAT_R -showxrefs -showsyns -showdefs"

alias uberalles='blip -r uberon_with_isa -r xenopus_anatomy -r mouse_anatomy -r gemina_anatomy -r amphibian_anatomy -r cell -r fly_anatomy -r zebrafish_anatomy -r fma_downcase -r bila -r miaa'

alias gotax='go -r gotax -r taxslim -rel only_in_taxon -rel subclass -rel part_of'

alias viz-mammal='blip-vizualize-uberon -r fma_simple -r mouse_anatomy -rel subclass -rel part_of -to display'
alias viz-vert-hmz='blip-vizualize-uberon -r fma_simple -r mouse_anatomy -r zebrafish_anatomy -rel subclass -rel part_of -to display'
alias viz-vert-zx='blip-vizualize-uberon  -r zebrafish_anatomy -r xenopus_anatomy -rel subclass -rel part_of -to display'
alias viz-vert='blip-vizualize-uberon -r fma_simple -r mouse_anatomy -r zebrafish_anatomy -r xenopus_anatomy -rel subclass -rel part_of -to display'

export GO_XP_CHEBI_ARGS=' -r chebi -r goxp/biological_process_xp_chebi  -r goxp/molecular_function_xp_chebi'
export GO_XP_CL_ARGS=' -r cell -r goxp/biological_process_xp_cell'
export GO_XP_PRO_ARGS=' -r protein -r goxp/biological_process_xp_protein -r goxp/molecular_function_xp_protein'
export GO_XP_MA_ARGS=' -r mouse_anatomy -r goxp/biological_process_xp_mouse_anatomy'
export GO_XP_SELF_ARGS=' -r goxp/biological_process_xp_self -r goxp/biological_process_xp_cellular_component -r goxp/biological_process_xp_multi_organism_process -r goxp/cellular_component_xp_self -r goxp/cellular_component_xp_go -r goxp/cellular_component_xp_cell -r goxp/molecular_function_xp_cellular_component -r goxp/molecular_function_xp_regulators -r goxp/molecular_function_xp_biological_process -r goxp/biological_process_xp_molecular_function -r obolr/biological_process_xp_regulation'
export GO_XP_FMA_ARGS=' -r fma -r goxp/biological_process_xp_human_anatomy'
export GO_XP_UBER_ARGS=' -r fma -r goxp/biological_process_xp_uber_anatomy -r uberon'
export GO_XP_PO_ARGS=' -r fma -r goxp/biological_process_xp_plant_anatomy'
export GO_XP_STIM_ARGS=' -r goxp/stimulus -r goxp/biological_process_xp_stimulus'
export GO_XP_ARGS="$GO_XP_CL_ARGS $GO_XP_CHEBI_ARGS $GO_XP_SELF_ARGS $GO_XP_STIM_ARGS $GO_XP_UBER_ARGS $GO_XP_PO_ARGS $GO_XP_PRO_ARGS"
alias gox="ov -r go $GO_XP_ARGS -showxp"
alias qgox="oq -r go $GO_XP_ARGS -showxp"
alias goxcell="ov -r go $GO_XP_CL_ARGS -showxp"
alias qgoxcell="oq -r go $GO_XP_CL_ARGS -showxp"
alias goxmouse="ov -r go $GO_XP_CL_ARGS -showxp"
alias qgoxmouse="oq -r go $GO_XP_CL_ARGS -showxp"
alias goxhuman="ov -r go $GO_XP_CL_ARGS -showxp"
alias qgoxhuman="oq -r go $GO_XP_CL_ARGS -showxp"
alias goxu="ov -r go $GO_XP_UBER_ARGS -showxp"
alias qgoxu="oq -r go $GO_XP_UBER_ARGS -showxp"

alias viz-goxu='blip -id GO:0060977 -to display -u ontol_entailment_basic -to display'

alias blip-viz='blip ontol-subset -to display'
alias blip-viz-upheno='blip -u ontol_config_upheno -u ontol_config_hidexref -u ontol_manifest_reverse_relation_from_uberon_xref  -u ontol_manifest_relation_from_uberon_xref ontol-subset -rel xref -cr xrefed_by -rel subclass'

alias go+db="gox -u ontol_sqlmap_go -bindsql 'curation_db:curation_statement/4-go' -showannots"
alias blip-go-sql="blip -u ontol_db -u blipkit_sql -u ontol_sqlmap_go -u seqfeature_sqlmap_go -u homol_sqlmap_go -u curation_db -u seqfeature_db"

alias fly2fma="ov -r fly_anatomy -r fma -r fly2fma"
alias zf2fma="ov -r zebrafish_anatomy -r fma -r zf2fma"
alias mouse2fma="ov -r mouse_anatomy -r fma -r mouse2fma"
alias all2fma="ov -r mouse_anatomy -r fma -r fly_anatomy -r zebrafish_anatomy -r mouse2fma -r zf2fma -r fly2fma"

alias wpx="ov -r worm_phenotype -r worm_phenotype_xp -r worm_anatomy -r go -r pato -showxp"
alias qwpx="oq -r worm_phenotype -r worm_phenotype_xp -r worm_anatomy -r go -r pato -showxp"

alias go-to-class='blip -r so -r go -include "association ontology" -include "feature ontology" -f go_assoc io-convert -u goa_bridge_to_class -u seqfeature_bridge_to_class -u ontol_db -to obo -i'
alias chaos-to-class='blip -r soxp -include "feature ontology" -f chaos io-convert -u seqfeature_bridge_to_class -u ontol_db -to obo -i'

alias pathviz='blip -r go pathway-viz'
alias reactome='blip -r reactome/Homo_sapiens -r go pathway-viz'


# --convenience aliases for using obol--
alias obol-plant-trait="obol -u obol_plant_trait_xp -r plant_trait -r ubo -r relationship -r pato -r spatial -r plant_anatomy -r go -r xchebi -r plant_environment"
alias obol-plant-anat="obol -r plant_anatomy -r ubo -r relationship -r spatial -table_pred user:gross_anatomical/3 -table_pred user:gross_anatomical5/3 -u obol_anatomy_xp"
alias obol-plant-env="obol -r plant_environment -r ubo -r relationship -r spatial -r chebi"
#alias obol-mpo="obol -r ubo -r relationship -r pato -r spatial -r mammalian_phenotype -r mouse_anatomy -r cell -r xchebi -r ontol_wn -r wn2ubo"
alias obol-mpo="obol -r ubo -r relationship -r pato -r spatial -r mouse_anatomy -r go -r cell -r xchebi -r caro"
alias obol-mp-core="obol -u obol_mammalian_phenotype_xp -r relationship -r spatial -r pato -r mammalian_phenotype -r mouse_anatomy -table_pred classdef_parser:any_kind_of/3 -table_pred ontol_db:subclassT/2  -table_pred user:process/3 -table_pred user:process5/3 -table_pred user:bearer/3 -table_pred user:quality/3 -table_pred gross_anatomical/3   -table_pred user:phenotype/3 -table_pred gross_anatomical5/3"
alias obol-rkc-core="obol -u obol_rkc_phenotype_xp -r relationship -r spatial -r pato -r go -r xchebi -r rkc -table_pred classdef_parser:any_kind_of/3 -table_pred ontol_db:subclassT/2  -table_pred user:process/3 -table_pred user:process5/3 -table_pred user:bearer/3 -table_pred user:quality/3 -table_pred gross_anatomical/3   -table_pred user:phenotype/3 -table_pred gross_anatomical5/3"
alias obol-wp-core="obol -u obol_worm_phenotype_xp -r relationship -r spatial -r pato -r worm_phenotype -r worm_anatomy -table_pred user:any_kind_of/3 -table_pred user:subclassT/2"

alias obol-go="obol -r biological_role -r ubo -r relationship -r spatial -r go -r pato -r cell -r xchebi -r fly_anatomy -r plant_anatomy -r zebrafish_anatomy -r mouse_anatomy"
alias obol-go-core="obol -r go_synonyms -r ubo -r relationship -r ro_proposed -r spatial -r go -table_pred user:process5/3 -table_pred user:process/3 -table_pred classdef_parser:any_kind_of/3 -table_pred ontol_db:subclassT/2"
alias obol-go-cell="obol-go-core -u obol_biological_process_xp_cell -r cell"
alias obol-go-anatomy="obol-go-core -u obol_biological_process_xp_anatomy  -r fly_anatomy -r plant_anatomy -r zebrafish_anatomy"
alias obol-go-chemical="obol-go-core -u obol_biological_process_xp_chemical  -u obol_molecular_function_xp_chemical -r xchebi"
alias obol-go-cc="obol-go-core -u obol_biological_process_xp_cellular_component"
alias obol-go-self="obol-go-core -u obol_biological_process_xp_self"
alias obol-go-quality="obol-go-core -u obol_biological_process_xp_quality -r pato"
#alias obol-go-all="obol-go-core -u obol_go_xp_all -r pato -r cell -r xchebi -r fly_anatomy -r plant_anatomy -r zebrafish_anatomy -r mouse_anatomy"
alias obol-go-all="obol-go-core -u obol_go_xp_all -r pato -r cell -r xchebi"

# only need MA until we have complete FMA??
alias obol-disease="obol -r caro -r ubo -r relationship -r spatial -r go -r fma_stemmed -r cell -r disease_stemmed -r pato -r mammalian_phenotype -r xchebi -r go"
alias obol-fly="obol -r ubo -r relationship -r spatial -r caro -r fly_anatomy -r go -r cell"
alias obol-fly-phenotype="obol -r ubo -r relationship -r spatial -r fly_anatomy -r cell -r pato -r go"
alias obol-fma="obol -r ubo -r relationship -r spatial -r cell"
alias obol-cell="obol -r ubo -r relationship -r spatial -r caro -r xchebi -r go"
#alias obol-worm-phenotype="obol -r ubo -r relationship -r spatial -r worm_anatomy -r xchebi -r cell -r pato -r go"

alias obol-reasoner-go-cell="blip-ddb -r implied/go -r implied/pato -r implied/cell -r implied/xchebi ontol-reasoner"

export NCI=http://ncicb.nci.nih.gov/xml/owl/EVS/Thesaurus.owl
