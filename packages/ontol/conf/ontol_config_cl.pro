
:- multifile user:graphviz_ontol_param/2.

user:graphviz_ontol_param(edge(_,_,has_low_plasma_membrane_amount,_),arrowhead=tee).
user:graphviz_ontol_param(edge(_,_,has_low_plasma_membrane_amount,_),color=red).
user:graphviz_ontol_param(edge(_,_,has_low_plasma_membrane_amount,_),penwidth=2).
user:graphviz_ontol_param(edge(_,_,has_low_plasma_membrane_amount,_),override(label='LOW')).

user:graphviz_ontol_param(edge(_,_,has_plasma_membrane_part,_),arrowhead=ediamond).
user:graphviz_ontol_param(edge(_,_,has_plasma_membrane_part,_),penwidth=3).
user:graphviz_ontol_param(edge(_,_,has_plasma_membrane_part,_),color=green).
user:graphviz_ontol_param(edge(_,_,has_plasma_membrane_part,_),override(label='HAS')).

user:graphviz_ontol_param(edge(_,_,is_a,_),override(label='I')).
%user:graphviz_ontol_param(edge(_,_,preceded_by,_),label='>').
