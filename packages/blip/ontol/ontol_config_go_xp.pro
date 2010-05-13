
:- multifile user:graphviz_ontol_param/2.

nice_color(lemonchiffon).
nice_color(steelblue2).
nice_color(peru).
nice_color(yellow).
nice_color(hotpink).
nice_color(green).


user:graphviz_ontol_param(node(_),style=filled).
user:graphviz_ontol_param(node(X),fillcolor=lemonchiffon):- ontol_db:belongs(X,chebi_ontology).
user:graphviz_ontol_param(node(X),fillcolor=steelblue2):- ontol_db:belongs(X,biological_process).


