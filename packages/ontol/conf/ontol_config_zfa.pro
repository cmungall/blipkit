:- [ontol_config_default].

:- multifile user:graphviz_ontol_param/2.
user:graphviz_ontol_param(containment_relation('zebrafish_anatomical_ontology'),part_of).
user:graphviz_ontol_param(display_relation('zebrafish_anatomical_ontology'),part_of).
user:graphviz_ontol_param(display_relation('zebrafish_anatomical_ontology'),develops_from).
user:graphviz_ontol_param(display_relation('zebrafish_anatomical_ontology'),subclass).

user:graphviz_ontol_param(edge(_,_,preceded_by,_),arrowhead=ediamond).
user:graphviz_ontol_param(edge(_,_,preceded_by,_),color=green).
