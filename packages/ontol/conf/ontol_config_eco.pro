:- use_module(bio(metadata_db)).
:- use_module(bio(ontol_db)).

:- [ontol_config_default].

        
:- multifile user:graphviz_ontol_param/2.


user:graphviz_ontol_param(node(X),fillcolor=lightblue) :- class(R,'automatic assertion'),parentT(X,R).
user:graphviz_ontol_param(node(X),fillcolor=pink) :- class(R,'manual assertion'),parentT(X,R).

user:graphviz_ontol_param(node(_),style=filled).


