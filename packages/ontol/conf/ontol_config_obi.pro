:- multifile user:graphviz_ontol_param/2.
%user:graphviz_ontol_param(containment_relation(obi),subclass).
% TODO: by id-space
%user:graphviz_ontol_param(display_relation('http://purl.obolibrary.org/obo/'),'http://purl.obolibrary.org/obo/OBI_0000308').
user:graphviz_ontol_param(display_relation('http://purl.obolibrary.org/obo/'),'OBI:0000308'). % realizes
user:graphviz_ontol_param(display_relation('http://purl.obolibrary.org/obo/'),'OBO_REL:has_part'). % 
user:graphviz_ontol_param(display_relation('http://purl.obolibrary.org/obo/'),'http://purl.org/obo/owl/OBO_REL#bearer_of'). % 

user:graphviz_ontol_param(edge(_,_,'OBO_REL:has_part',_),label='has part').
user:graphviz_ontol_param(edge(_,_,'OBO_REL:has_part',_),color=blue).
user:graphviz_ontol_param(edge(_,_,'OBI:0000308',_),color=green).
user:graphviz_ontol_param(edge(_,_,'OBI:0000308',_),arrowhead=tee).
