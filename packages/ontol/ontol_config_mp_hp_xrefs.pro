:- multifile user:graphviz_ontol_param/2.

:- use_module(bio(metadata_db)).

x_ont(ID,Ont) :-
	entity_xref(ID,X),
	id_idspace(X,Ont).

	

user:graphviz_ontol_param(node(_),style=filled).

user:graphviz_ontol_param(node(X),fillcolor=lemonchiffon):-
	x_ont(X,'MP'),
	x_ont(X,'HP').

user:graphviz_ontol_param(node(X),fillcolor=red):-
	x_ont(X,'MP'),
	\+ x_ont(X,'HP').

user:graphviz_ontol_param(node(X),fillcolor=blue):-
	\+ x_ont(X,'MP'),
	x_ont(X,'HP').


