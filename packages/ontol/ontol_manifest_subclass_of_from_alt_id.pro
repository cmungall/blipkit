:- module(ontol_manifest_subclass_of_from_xref,[]).

:- use_module(bio(metadata_db)).
:- use_module(bio(ontol_db)).

:- multifile ontol_db:subclass/2.
ontol_db:subclass(Y,X):- entity_alternate_identifier(X,Y),X\=Y,class(Y). % no dangling


        
