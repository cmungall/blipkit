:- module(query_resolver,
	  [resolve_query/3]).

:- multifile resolve_query_hook/4.


%% resolve_query+(DataType,+QueryTerms:list,?Results:list,?UnresolvedList:list)
%
% DataType = entity | class | pathway | ....
%  (hooks must be provided)
%
%
% QueryTerm =
%  * ids(IDs:list) |
%  * names(Names:list) |
resolve_query(DataType,QueryTerms,Results,UnresolvedList) :-
	member(ids(QIDs),QueryTerms),
	nonvar(QIDs),
        QIDs = [_|_],
        !,
        resolve_query(DataType,id,QIDs,Results,UnresolvedList).

resolve_query(DataType,QueryTerms,Results,UnresolvedList) :-
        member(names(QNs),QueryTerms),
	nonvar(QNs),
        QNs = [_|_],
        !,
        resolve_query(DataType,name,QNs,Results,UnresolvedList).

%resolve_query(DataType,CIDs,_,CIDs) :- !.

resolve_query(DataType,QueryTerms,Results,UnresolvedList) :-
        member(idfile(F),QueryTerms),
        nonvar(F),
        !,
        read_file_to_tokens(F,QIDs),
        resolve_query(DataType,id,QIDs,Results,UnresolvedList).

resolve_query(DataType,Key,Qs,Results,UnresolvedList) :-
	is_list(Qs),
	!,
	setof(Q-R,
	      (	  member(Q,Qs),
		  resolve_query_hook(DataType,Key,Q,R)),
	      ResultMap),
        !,
        lookupmap_ids(Qs,ResultMap,Results,UnresolvedList).



% given a map from the query set to the result set, partition this
% into results and unresolved list
% TODO: ambiguous
lookupmap_ids(QIDs,Map,PIDs,NoIDs) :-
        findall(PID,member(QID-PID,Map),PIDs),
        findall(QID,(member(QID,QIDs),\+member(QID-_,Map)),NoIDs).

:- module(query_resolver,[]).

/** <module> resolution of generic queries

  ---+ Synopsis

---+ Details

TODO - IN PROGRESS

  this will eventually be a generic hookable query resultion module

  pass in a query - e.g. a list of IDs, list of names, ID file

  get back a list of results, and a mapping of queries to results, plus optionally
  unresolved query elements and ambiguous query elements
  
*/
