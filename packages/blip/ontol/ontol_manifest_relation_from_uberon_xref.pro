:- module(ontol_manifest_relation_from_uberon_xref,[]).

:- use_module(bio(ontol_db)).

:- multifile ontol_db:parent_over_nr/3.
ontol_db:parent_over_nr(xref,X,Y):-
	class_xref(X,Y),class(Y),
	id_idspace(X,S),
	(   S='UBERON'
	;   S='UPHENO').




        
