:- module(ontol_manifest_subclass_of_from_xref,[]).

:- use_module(bio(ontol_db)).

:- multifile ontol_db:subclass/2.
ontol_db:subclass(X,Y):- class_xref(X,Y),class(Y). % no dangling


        
