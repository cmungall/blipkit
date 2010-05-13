:- use_module(bio(metadata_db)).


:- multifile user:graphviz_ontol_param/2.


user:graphviz_ontol_param(node(_),style=filled).
user:graphviz_ontol_param(node(X),fillcolor=steelblue2):- sub_atom(X,_,_,_,'^').
user:graphviz_ontol_param(node(X),fillcolor=peru):- id_idspace(X,'TTO').
user:graphviz_ontol_param(node(X),fillcolor=lemonchiffon):- entity_xref(X,Y),id_idspace(Y,'ZFA').
user:graphviz_ontol_param(node(X),fillcolor=pink):- id_idspace(X,'TAO'), \+((entity_xref(X,Y),id_idspace(Y,'ZFA'))),\+sub_atom(X,_,_,_,'^').




