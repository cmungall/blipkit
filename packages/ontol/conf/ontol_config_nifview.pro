:- use_module(bio(metadata_db)).
:- multifile user:graphviz_ontol_param/2.
user:graphviz_ontol_param(edge(_,_,part_of,_),override(label='P')).
user:graphviz_ontol_param(edge(_,_,part_of,_),color=blue).
user:graphviz_ontol_param(edge(_,_,po,_),override(label='P')).
user:graphviz_ontol_param(edge(_,_,po,_),color=blue).
%user:graphviz_ontol_param(edge(_,_,preceded_by,_),label='>').
user:graphviz_ontol_param(node(X),fillcolor=yellow) :- id_idspace(X,'UBERON').
user:graphviz_ontol_param(node(X),style=filled) :- id_idspace(X,'UBERON').
%user:graphviz_ontol_param(node(X),style=filled) :- entity_xref_idspace(X,_,'EFO'). %entity_partition(X,efo_slim).
%user:graphviz_ontol_param(node(X),override(shape=oval)) :- entity_xref_idspace(X,_,'EFO').

        

