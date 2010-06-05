:- module(ontol_manifest_abduced_links,[manifest/0]).

:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).

:- multifile ontol_db:subclass/2.

manifest:- forall(abduced_link(subclass(X,Y)),
                  assert(ontol_db:subclass(X,Y))).



        
