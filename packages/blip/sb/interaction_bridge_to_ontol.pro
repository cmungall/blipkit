:- module(interaction_bridge_to_ontol,
          []).

:- use_module(interaction_db).
:- use_module(bio(ontol_db),[]).

ontol_db:inst_rel(X,interacts_with,Y) :- interacts_with(X,Y).



