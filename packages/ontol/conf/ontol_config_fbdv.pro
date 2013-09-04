
:- multifile user:graphviz_ontol_param/2.
user:graphviz_ontol_param(containment_relation('FlyBase development CV'),part_of).
user:graphviz_ontol_param(display_relation('FlyBase development CV'),all).

%user:graphviz_ontol_param(edge(_,_,preceded_by,_),arrowtail=ediamond).
user:graphviz_ontol_param(edge(_,_,preceded_by,_),arrowhead=ediamond).
user:graphviz_ontol_param(edge(_,_,preceded_by,_),color=green).
user:graphviz_ontol_param(edge(_,_,preceded_by,_),override(label='>')).
%user:graphviz_ontol_param(edge(_,_,preceded_by,_),label='>').
