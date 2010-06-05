:- module(pathway_lookup,
	  [
	   query_pathways/3
	  ]).

:- use_module(pathway_db).
:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).

:- multifile query_resolver:resolve_query_hook/4.

query_resolver:resolve_query_hook(pathway,id,Q,R) :-
	lookup_pathway_by_id(Q,R).


% ID RESOLUTION
% TODO: make more generic
% TODO: ambiguous ID/name policy

%% query_pathways(+QueryTerms:list,?PIDs:list,?Unresolved:list)
% QueryTerm =
%  ids(IDs:list) |
%  names(Names:list) |
%  idfile(FileName)
query_pathways(Opts,PIDs,NoIDs) :-
        member(ids(QIDs),Opts),
	nonvar(QIDs),
        QIDs=[_|_],
        !,
        lookup_pathway_by_ids(QIDs,PIDs,NoIDs).

query_pathways(Opts,PIDs,NoIDs) :-
        member(names(QNs),Opts),
	nonvar(QNs),
        QNs=[_|_],
        !,
        lookup_pathway_by_names(QNs,PIDs,NoIDs).

query_pathways(PIDs,_,PIDs) :- !.

query_pathways(PIDs,Opts,NoIDs) :-
        member(idfile(F),Opts),
        nonvar(F),
        !,
        read_file_to_tokens(F,QIDs),
        lookup_pathway_by_ids(QIDs,PIDs,NoIDs).

%% lookup_pathway_by_ids(+QIDs:list, ?PIDs:list, ?NoIDs:list)
lookup_pathway_by_ids(QIDs,PIDs,NoIDs) :-
        setof(QID-PID,(member(QID,QIDs),lookup_pathway_by_id(QID,PID)),Map),
        !,
        lookupmap_ids(QIDs,Map,PIDs,NoIDs).
lookup_pathway_by_ids(QIDs,[],QIDs).

%% lookup_pathway_by_names(+QNames:list, ?PIDs:list, ?NoIDs:list)
lookup_pathway_by_names(QNs,PIDs,NoIDs) :-
        setof(QN-PID,(member(QN,QNs),lookup_pathway_by_name(QN,PID)),Map),
        !,
        lookupmap_ids(QNs,Map,PIDs,NoIDs).
lookup_pathway_by_names(QNs,[],QNs).

%% lookup_pathway_by_id(+QID,?PID)
% QID can be
%  * event ID
%  * event xref ID
%  * GO ID (subclassRT/2 closure used)
lookup_pathway_by_id(QID,QID) :-
	debug(lookup,'Checking of event(~w)',[QID]),
        event(QID),
        !.

lookup_pathway_by_id(QID,PID) :-
	debug(lookup,'Checking of event xref(~w)',[QID]),
        entity_xref(PID,QID),
        event(PID).
%        \+ \+ subpathway_of(_,PID).


% For example, by GO ID.
% use subclass inferences
lookup_pathway_by_id(QID,PID) :-
	subclassRT(QID_A,QID),
	entity_xref(PID,QID_A),
	event(PID).
% any relation TODO
%lookup_pathway_by_id(QID,PID) :-
%	parentT(QID_A,QID),
%	entity_xref(PID,QID_A),
%	event(PID).

% slow:
%lookup_pathway_by_id(QID,PID) :-
%	parent_overT(part_of,QID_A,QID),
%	entity_xref(PID,QID_A),
%	event(PID).

%lookup_pathway_by_id(QID,PID) :-
%	 debug(lookup,'Checking of event xref(~w)',[QID]),
%        entity_xref(C,QID),
%        snapshot_continuant(M,C),
%        event_participant_roleT(PID,M,_).

% participants
lookup_pathway_by_id(QID,PID) :-
	debug(lookup,'Checking for participants xref(~w)',[QID]),
	subclassRT(QID_A,QID),
	entity_xref(CID,QID_A),
	snapshot_continuant(S,CID),
	event_participant_role(PID,S,_).

lookup_pathway_by_name(N,PID) :-
        entity_label(QID,N),
        lookup_pathway_by_id(QID,PID).

% given a map from the query set to the result set, partition this
% into results and unresolved list
lookupmap_ids(QIDs,Map,PIDs,NoIDs) :-
        findall(PID,member(QID-PID,Map),PIDs),
        findall(QID,(member(QID,QIDs),\+member(QID-_,Map)),NoIDs).

/** <module> 

---+ Details



*/
