:- use_module(bio(metadata_db)).
:- use_module(bio(ontol_db)).

:- [ontol_config_default].

        
:- multifile user:graphviz_ontol_param/2.

ont_col('OMIM',red).
ont_col('Orphanet',yellow).


user:graphviz_ontol_param(edge(_,_,equivalent,_),penwidth=3).
user:graphviz_ontol_param(edge(_,_,equivalent,_),style=dash).
user:graphviz_ontol_param(edge(_,_,equivalent,_),color=steelblue).
user:graphviz_ontol_param(edge(_,_,equivalent,_),override(label='=')).
user:graphviz_ontol_param(node(_),style=filled).

user:graphviz_ontol_param(node(key),label=key).
user:graphviz_ontol_param(node(X),fillcolor=C):- id_idspace(X,O),ont_col(O,C).




