/* -*- Mode: Prolog -*- */

:- module(ontol_bridge_from_bp_mappings,
          [
	   mapping_source_target/3,
	   compare_mapping/4
          ]).

:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).
:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdfs')).

metadata_db:entity_xref(S,T) :- mapping_source_target(_,S,T).

mapping_source_target(M,S,T) :-
	rdf_has(M,'http://protege.stanford.edu/mappings#source',Sx),
	bpuri_id(Sx,S),
	rdf_has(M,'http://protege.stanford.edu/mappings#target',Tx),
	bpuri_id(Tx,T).

bpuri_id(X,ID) :-
	concat_atom(L,'/',X),
	reverse(L,[IDx|_]),
	mapid(IDx,ID).

mapid(ID,ID) :- concat_atom([_,_],':',ID),!.
mapid(N,ID) :- concat_atom(Toks,'_',N),concat_atom(Toks,' ',N2),entity_label(ID,N2),!.

% TODO - move
u3(U,S,T) :-
	entity_xref(U,S),
	\+ \+ entity_label(S,_),
	id_idspace(U,'UBERON'),
	entity_xref(U,T),
	S\=T,
	\+ \+ entity_label(T,_).
	
compare_mapping(S,T,M,U) :-
	mapping_source_target(M,S,T),
	u3(U,S,T).
compare_mapping(S,T,M,'NO_MAPPING') :-
	mapping_source_target(M,S,T),
	\+ u3(_,S,T).
compare_mapping(S,T,'NO_MAPPING',U) :-
	u3(U,S,T),
	\+mapping_source_target(_,S,T).

	

	