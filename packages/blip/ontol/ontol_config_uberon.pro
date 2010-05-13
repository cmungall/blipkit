:- use_module(bio(metadata_db)).
:- use_module(bio(ontol_db)).

:- multifile user:graphviz_ontol_param/2.

ont_col('MA',lemonchiffon).
ont_col('ZFA',green).
ont_col('XAO',peru).
ont_col('FBbt',yellow).
ont_col('AAO',hotpink).
ont_col('FMA',steelblue2).

xont_col(O,C) :- ont_col(O,C),\+ \+((class(X),id_idspace(X,O))).


user:graphviz_ontol_param(display_relation(uberon),part_of).
user:graphviz_ontol_param(display_relation(uberon),develops_from).
user:graphviz_ontol_param(edge(X,_,is_a,_),penwidth=3) :- id_idspace(X,'UBERON').
user:graphviz_ontol_param(edge(X,_,is_a,_),weight=50) :- id_idspace(X,'UBERON').
user:graphviz_ontol_param(node(_),style=filled).
user:graphviz_ontol_param(node(X),fillcolor=C):- id_idspace(X,O),ont_col(O,C).

user:graphviz_ontol_param(key(X),fillcolor=Col) :- xont_col(X,Col).
user:graphviz_ontol_param(key(X),label=X) :- xont_col(X,_).
user:graphviz_ontol_param(key(X),style=filled) :- xont_col(X,_).
user:graphviz_ontol_param(key(key),label=key).


