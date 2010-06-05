:- module(relation_closure,
	  [
	   node_relation_closure/3,
	   nodes_relation_closure/3,
	   nodes_relation_closure/5
	   ]).

:- use_module(library(ordsets)).

:- module_transparent nodes_relation_closure/3.
:- module_transparent nodes_relation_closure/5.

%% node_relation_closure(+ID, +Rel:atom, ?AllAncs:set) is det
% if ID is var, is semidet
node_relation_closure(ID,Rel,Ancs) :-
	nonvar(ID),
	!,
	nodes_relation_closure([ID],Rel,Ancs).
node_relation_closure(ID,Rel,Ancs) :-
	Goal=..[Rel,ID,XID],
	setof(ID,XID^Goal,IDs),
	member(ID,IDs),
	nodes_relation_closure([ID],Rel,Ancs).


%% nodes_relation_closure(+IDs:list, +Rel:atom, ?AllAncs:set) is det
% Rel is the name of a binary predicate, e.g. subclassof
nodes_relation_closure(IDs,Rel,AllAncs) :-
	nodes_relation_closure(IDs,Rel,[],[],AncsFinal),
	sort(IDs,IDsSorted),
	ord_union(IDsSorted,AncsFinal,AllAncs).

%% nodes_relation_closure(+IDs:list, +Rel:atom, +DoneIDs:list, +Ancs:set, ?AllAncs:set) is det
nodes_relation_closure([ID|IDs],Rel,DoneIDs,Ancs,AncsFinal) :-
	debug(closure,'closure(~w) todo: ~w done:~w answer_so_far:~w',[Rel,IDs,DoneIDs,Ancs]),
	Goal=..[Rel,ID,XID],
	setof(XID,Goal,Parents),
	!,
	ord_union(Parents,IDs,U),
	sort(DoneIDs,DoneIDsSorted),
	ord_subtract(U,DoneIDsSorted,NextIDs),
	ord_union(Ancs,Parents,AncsNew),
	nodes_relation_closure(NextIDs,Rel,[ID|DoneIDsSorted],AncsNew,AncsFinal).
nodes_relation_closure([ID|IDs],Rel,DoneIDs,Ancs,AncsFinal) :-
	!,
	nodes_relation_closure(IDs,Rel,[ID|DoneIDs],Ancs,AncsFinal).
nodes_relation_closure([],_,_,Ancs,Ancs).
