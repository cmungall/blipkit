:- module(ontol_manifest_has_subclass_from_xref,[]).

:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).

:- multifile ontol_db:subclass/2.
:- multifile ontol_db:class/2.
%ontol_db:subclass(X,Y):- class_xref(Y,X).
ontol_db:subclass(X,Y):- entity_xref(Y,X),\+entity_obsolete(Y,_).
ontol_db:class(X):- entity_xref(Y,X),\+entity_obsolete(Y,_).

        
