
:- module(pkb_to_ontol,
	  []).

:- use_module(pkb_db).

metadata_db:entity_label(E,L) :- organism_label(E,L).
