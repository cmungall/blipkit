:- use_module(bio(metadata_db)).
:- use_module(bio(ontol_db)).

:- [ontol_config_default].

        
:- multifile user:graphviz_ontol_param/2.

ont_col('MA',lemonchiffon).
ont_col('FMA',yellow).


user:graphviz_ontol_param(edge(_,_,equivalent,_),weight=0).
user:graphviz_ontol_param(edge(_,_,subclass,_),weight=50).
user:graphviz_ontol_param(edge(_,_,equivalent,_),penwidth=3).
user:graphviz_ontol_param(edge(_,_,equivalent,_),style=dash).
user:graphviz_ontol_param(edge(_,_,equivalent,_),color=steelblue).
user:graphviz_ontol_param(edge(_,_,equivalent,_),override(label='=')).
user:graphviz_ontol_param(node(_),style=filled).




