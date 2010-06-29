:- multifile user:graphviz_ontol_param/2.

:- use_module(bio(metadata_db)).

x_ont(ID,Ont) :-
	entity_xref(ID,X),
	id_idspace(X,Ont).

	

user:graphviz_ontol_param(node(_),style=filled).

user:graphviz_ontol_param(node(X),fillcolor=lemonchiffon):-
	id_idspace(X,'GOCHE'),
	x_ont(X,'CHEBI').

user:graphviz_ontol_param(node(X),fillcolor=green):-
	id_idspace(X,'GOCHE'),
	\+ x_ont(X,'CHEBI').

user:graphviz_ontol_param(node(X),fillcolor=blue):-
	id_idspace(X,'CHEBI').



