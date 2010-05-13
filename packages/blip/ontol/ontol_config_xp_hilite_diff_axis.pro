
:- multifile user:graphviz_ontol_param/2.

user:graphviz_ontol_param(edge(A,B,is_a,_),penwidth=5):-
	genus(A,GA),
	genus(B,GB),
	subclassRT(GA,GB),
	differentium(A,_,XA),
	differentium(B,_,XB),
	subclassT(XA,XB).


	


