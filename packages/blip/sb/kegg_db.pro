:- module(kegg_db,
	  [
	   kpathway/4,
	   knode/3,
	   knode_pathway/2,
	   subcomponent_of/2,
	   kedge/3,
	   kreaction/2,
	   ksubstrate/2,
	   kproduct/2,
	   graphics/6,
	   kpathway_ugraph/2
	   ]).


:- use_module(bio(dbmeta)).
:- use_module(bio(metadata_db)).
:- use_module(library(ugraphs)).

%% kpathway(Name,Org,Number,Title)
:- extensional(kpathway/4).

%% knode(PID,Name,Type)
:- extensional(knode/3).

:- extensional(knode_pathway/2).

%% subcomponent_of(CID,PID)
:- extensional(subcomponent_of/2).

%% kedge(T,PID1,PID2)
:- extensional(kedge/3).

:- extensional(kreaction/2).
:- extensional(ksubstrate/2).
:- extensional(kproduct/2).


:- extensional(graphics/6).

%% kpathway_ugraph(+P,?UG)
% see ugraphs.pl
%
% Example:
% ==
% kpathway_graph(P,G),transitive_closure(G,GC)
% ==
%
% ==
% kpathway_graph(P,G),reachable('path:hsa05010-1',G,VL)
% ==
kpathway_ugraph(P,UG) :-
	setof(A-B,T^(knode_pathwat(A,P),kedge(T,A,B)),VEs),
	vertices_edges_to_ugraph([],VEs,UG).
