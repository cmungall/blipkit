:- module(ontol_manifest_has_subclass_from_xref_nd,[]).

:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).

:- multifile ontol_db:subclass/2.
ontol_db:subclass(X,Y):- entity_xref(Y,X),class(X).

        
