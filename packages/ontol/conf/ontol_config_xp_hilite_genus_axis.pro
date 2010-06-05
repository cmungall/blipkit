
:- multifile user:graphviz_ontol_param/2.

user:graphviz_ontol_param(edge(A,B,is_a,_),penwidth=5):-
	differentium(A,_,X),
	differentium(B,_,X).

	


