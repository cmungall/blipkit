
:- multifile user:graphviz_ontol_param/2.

user:graphviz_ontol_param(edge(_,_,inheres_in,_),arrowhead=box).
user:graphviz_ontol_param(edge(_,_,inheres_in,_),color=grey).
user:graphviz_ontol_param(edge(_,_,has_part,_),color=blue).
user:graphviz_ontol_param(edge(_,_,lacks_part,_),color=red).
user:graphviz_ontol_param(edge(_,_,lacks_part,_),arrowhead=tee).

user:graphviz_ontol_param(edge(_,_,property_of,_),arrowhead=box).
user:graphviz_ontol_param(edge(_,_,property_of,_),color=darkgrey).
user:graphviz_ontol_param(edge(_,_,not_property_of,_),color=red).
user:graphviz_ontol_param(edge(_,_,not_property_of,_),arrowhead=tee).

user:graphviz_ontol_param(edge(_,_,affects,_),arrowhead=box).
user:graphviz_ontol_param(edge(_,_,affects,_),color=darkgrey).
user:graphviz_ontol_param(edge(_,_,not_affects,_),color=red).
user:graphviz_ontol_param(edge(_,_,not_affects,_),arrowhead=tee).
