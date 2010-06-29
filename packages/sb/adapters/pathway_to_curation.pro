/* -*- Mode: Prolog -*- */

:- module(pathway_to_curation,[]).

:- use_module(bio(metadata_db)).
:- use_module(bio(ontol_db)).
:- use_module(bio(curation_db)).
:- use_module(bio(pathway_db)).
:- use_module(bio(pathway_go_util)).


curation_info(A,G,R,X,Src) :-
	event_catalyst(A,S),
	snapshot_continuant(S,C),
	entity_xref(C,U),
	id_idspace(U,'UniProtKB'),
	event_goxref(A,X),
	id_idspace(A,Src).


curation_db:curation_statement(A,G,R,X) :-
	curation_info(A,G,R,X,_).


	

