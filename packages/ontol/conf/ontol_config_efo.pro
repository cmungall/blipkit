:- use_module(bio(metadata_db)).
:- multifile user:graphviz_ontol_param/2.
%user:graphviz_ontol_param(containment_relation('FlyBase development CV'),part_of).
%user:graphviz_ontol_param(display_relation('FlyBase development CV'),all).

%user:graphviz_ontol_param(edge(_,_,preceded_by,_),arrowtail=ediamond).
%user:graphviz_ontol_param(edge(_,_,preceded_by,_),arrowhead=ediamond).
%user:graphviz_ontol_param(edge(_,_,preceded_by,_),color=green).
user:graphviz_ontol_param(edge(_,_,part_of,_),override(label='P')).
user:graphviz_ontol_param(edge(_,_,part_of,_),color=blue).
user:graphviz_ontol_param(edge(_,_,po,_),override(label='P')).
user:graphviz_ontol_param(edge(_,_,po,_),color=blue).
%user:graphviz_ontol_param(edge(_,_,preceded_by,_),label='>').
user:graphviz_ontol_param(node(X),fillcolor=yellow) :- entity_xref_idspace(X,_,'EFO'), entity_partition(X,efo_slim).
user:graphviz_ontol_param(node(X),fillcolor=grey) :- entity_xref_idspace(X,_,'EFO'), \+ entity_partition(X,efo_slim).
user:graphviz_ontol_param(node(X),style=filled) :- entity_xref_idspace(X,_,'EFO'). %entity_partition(X,efo_slim).

user:graphviz_ontol_param(node(X),override(shape=oval)) :- entity_xref_idspace(X,_,'EFO').

        

