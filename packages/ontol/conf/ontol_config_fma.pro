:- multifile user:graphviz_ontol_param/2.
user:graphviz_ontol_param(containment_relation(fma),subclass).
user:graphviz_ontol_param(display_relation(fma),regional_part_of).

user:graphviz_ontol_param(edge(_,_,regional_part_of,_),label='regional part of').
user:graphviz_ontol_param(edge(_,_,regional_part_of,_),arrowhead=ediamond).
user:graphviz_ontol_param(edge(_,_,regional_part_of,_),color=blue).
user:graphviz_ontol_param(edge(_,_,regional_part_of,_),weight=3).
user:graphviz_ontol_param(edge(_,_,systemic_part_of,_),color=red).
user:graphviz_ontol_param(edge(_,_,systemic_part_of,_),label='systemic part of').
user:graphviz_ontol_param(edge(_,_,systemic_part_of,_),style=dashed).
user:graphviz_ontol_param(edge(_,_,constitutional_part_of,_),color=green).
user:graphviz_ontol_param(edge(_,_,constitutional_part_of,_),label='constitutional part of').
