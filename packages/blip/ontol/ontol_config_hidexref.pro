:- use_module(bio(metadata_db)).

:- multifile user:graphviz_ontol_param/2, user:graphviz_ontol_param/3.

user:graphviz_ontol_param(edge(_,_,'xref',_),override(label='')).
user:graphviz_ontol_param(edge(_,_,'xref',_),color=transparent).





