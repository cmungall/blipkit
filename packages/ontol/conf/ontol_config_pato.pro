
:- multifile user:graphviz_ontol_param/2.
user:graphviz_ontol_param(containment_relation(quality),subclass).
user:graphviz_ontol_param(display_relation(quality),all).

user:graphviz_ontol_param(edge(_,_,reciprocal_of,_),arrowhead=ediamond).
user:graphviz_ontol_param(edge(_,_,reciprocal_of,_),arrowtail=ediamond).
user:graphviz_ontol_param(edge(_,_,reciprocal_of,_),color=blue).
user:graphviz_ontol_param(edge(_,_,decreased_in_magnitude_relative_to,_),color=red).
user:graphviz_ontol_param(edge(_,_,decreased_in_magnitude_relative_to,_),label='<').
user:graphviz_ontol_param(edge(_,_,increased_in_magnitude_relative_to,_),color=green).
user:graphviz_ontol_param(edge(_,_,increased_in_magnitude_relative_to,_),label='>').
