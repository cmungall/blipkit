/* -*- Mode: Prolog -*- */

:- module(metadata_mappings,
          [
           mapping/10,
	   mapping_source_target/3,
	   db_mapping_source_target/4,
	   compare_mapping/8
          ]).

:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).
:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdfs')).

:- use_module(bio(dbmeta)).

:- multifile exclude_class_hook/1.

%metadata_db:entity_xref(S,T) :- mapping_source_target(_,S,T).

mapping_source_target(M,S,T) :-
	db_mapping_source_target(_,M,S,T).

%% db_mapping_source_target(?Src,?Mapping,?Subj,?Targ)
% symmetric
% TODO - separate bridge files

% UBERON - M is the uberon class
db_mapping_source_target(uberon,M,S,T) :-
	u3(M,S,T),
	id_idspace(S,Sx),
	id_idspace(T,Tx),
	Sx\=Tx. % symmetric

% BP - M is the mapping ID
db_mapping_source_target(bp,M,S,T) :-
	bpmapping(M,S,T).
db_mapping_source_target(bp,M,S,T) :-
	bpmapping(M,T,S).

% asymmetric, normalized dir, S@<T
bpmapping(M,S,T) :-
	bpmapping_asym(M,S,T).
bpmapping(M,S,T) :-
	bpmapping_asym(M,T,S).

bpmapping_asym(M,S,T) :-
	rdf_has(M,'http://protege.stanford.edu/mappings#source',Sx),
	bpuri_id(Sx,S),
	rdf_has(M,'http://protege.stanford.edu/mappings#target',Tx),
	bpuri_id(Tx,T).

bpuri_id(X,ID) :-
	concat_atom(L,'/',X),
	reverse(L,[IDx|_]),
        debug(mapping,'finding ID for: ~w',[IDx]),
	mapid(IDx,ID),
        debug(mapping,'mapped ~w ==> ~w',[IDx,ID]).

mapid(ID,ID) :- concat_atom([_,_],':',ID),!.
mapid(IDx,ID) :- concat_atom([Pre,Local],'#',IDx),mapprefix(Pre,Pre2),concat_atom([Pre2,Local],:,ID),!.
mapid(N2,ID) :- atom_concat('fma3.0#',N,N2),concat_atom(Toks,'_',N),concat_atom(Toks,' ',N3),entity_label(ID,N3),!.

mapprefix('NIF-GrossAnatomy.owl','NIF_GrossAnatomy').




% TODO - move
u3(U,S,T) :-
	entity_xref(U,S),
	\+ entity_obsolete(U,_),
	\+ \+ entity_label(S,_), % must exist in DB
	id_idspace(U,'UBERON'),
	entity_xref(U,T),
	\+ \+ entity_label(T,_). % must exist in DB


% --

member_pair(X,Y,L) :-
	select(X,L,L2),
	member(Y,L2).


%% compare_mapping(+DBs:list,?Match,?Src,?Tgt,?Type,?AltMatch)
% given two or more mapping sources (e.g. bp and uberon), evaluate all matches in
% all sets.
% If both sources agree, then Type = 'EXACT' (and AltMatch in Info column)
% If one source finds a match, and the other does not, then Type=no_match(MSrc)
%  (and 'NULL' in INFO col)
% If the results partially disagree, then Type = src(Rel) | tgt(Rel)
% where Rel = parent|child
compare_mapping(DBs,DB,AltDB,Match,S,T,Type,Info) :-
	member_pair(DB,AltDB,DBs),
	asym_compare_mapping(DB,AltDB,Match,S,T,Type,Info),
	\+ exclude_class_hook(S),
	\+ exclude_class_hook(T).

asym_compare_mapping(DB,AltDB,Match,S,T,'EXACT',AltMatch) :- % alternate source agrees
	db_mapping_source_target(DB,Match,S,T),
	db_mapping_source_target(AltDB,AltMatch,S,T),
	S @< T.					      % remove dupes
asym_compare_mapping(DB,AltDB,Match,S,T,Type,Info) :- % alternate source agrees
	db_mapping_source_target(DB,Match,S,T),
	\+ db_mapping_source_target(AltDB,_,S,T), % same match not found
	db_pair_relationship(AltDB,S,T,Type,Info).		  % best match in Src2

% source matches something different
db_pair_relationship(_,S,_,src(obsolete),'OBS') :-
	entity_obsolete(S,_),
	!.
db_pair_relationship(_,_,T,tgt(obsolete),'OBS') :-
	entity_obsolete(T,_),
	!.
% target matches something different
db_pair_relationship(DB,S,T,tgt(R,T2),M) :-
	db_mapping_source_target(DB,M,S,T2),
	relt(T,T2,R).
% source matches something different
db_pair_relationship(DB,S,T,src(R,S2),M) :-
	db_mapping_source_target(DB,M,S2,T),
	relt(S,S2,R).
db_pair_relationship(uberon,S,T,lca,LCA) :-
	entity_xref(SX,S),
	entity_xref(TX,T),
	\+ entity_obsolete(SX,_),
	\+ entity_obsolete(TX,_),
	class_pair_subclass_lca(SX,TX,LCA),
	\+ exclude_class_hook(LCA).
		
% neither source not target match
db_pair_relationship(DB,S,T,no_match(DB),'NULL') :-
	\+ db_mapping_source_target(DB,_,_,T),
	\+ db_mapping_source_target(DB,_,S,_).


relt(X,Y,has_parent) :- bf_parentRT(X,Y),!.
relt(X,Y,has_child) :- bf_parentRT(Y,X),!.
relt(_,_,'NO_REL') :- !.


	

mapping(M,'','Automatic','','',S,SN,'',T,TN) :-
	rdf_has(M,'http://protege.stanford.edu/mappings#source',Sx),
	bpuri_id(Sx,S),
	rdf_has(M,'http://protege.stanford.edu/mappings#target',Tx),
	bpuri_id(Tx,T),
        class(S,SN),
        class(T,TN).


        
