:- module(ontol_manifest_links_from_obsoletion_metadata,[]).

:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).

:- multifile ontol_db:restriction/3.
ontol_db:restriction(X,consider,Y):- entity_consider(X,Y).
ontol_db:restriction(X,replaced_by,Y):- entity_replaced_by(X,Y).


        
