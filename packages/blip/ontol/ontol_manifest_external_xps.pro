:- module(ontol_manifest_external_xps,[]).

:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).

:- multifile ontol_db:restriction/3.
ontol_db:restriction(X,R,Y):- differentium(X,R,Y),id_idspace(X,XS),id_idspace(Y,YS),XS\=YS.



        
