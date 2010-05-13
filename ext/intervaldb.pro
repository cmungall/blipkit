:- module(intervaldb,
          [
           intervaldb_make/2,
           intervaldb_print/1,
           intervaldb_info/1,
           intervaldb_addone/4,
           nclist_build/2,
           nclist_intersection/4,
           nclist_intersection2/4,
           intervaldb_intersection/4,
           intervaldb_intersection2/4,
           intervaldb_match/3,
           intervaldb_match/5,
           intervaldb_match/4,
           intervaldb_match/7,
           intervaldb_addall/2,
           intervaldb_addone/3,
           intervaldb_free/1,
           read_intervals/3,
           spandb_make/1,
           spandb_addone/6,
           spandb_print/1,
           spandb_to_intervaldb/2,
           spandb_free/1,
           spandb2_make/1,
           spandb2_addone/6,
           spandb2_print/1,
           spandb2_to_intervaldb/2,
           spandb2_free/1,
           intervaldb_make/1,
           nclist_make/1,
           nclist_intersection_span/5,
           set_x/1,
           get_x/1,
           inc_x
           ]).

:- initialization load_foreign_library(intervaldb4pl).

%% span(?N:int,?Start:int,?End:int) is nondet
% extensional predicate (i.e. unit clause that is meant to be read in or asserted as data)

%% intervaldb_make(+N:int,-IDB) is det
% creates an IDB of size N

%% intervaldb_print(+IDB) is det
% prints out the all of the spans in an IDB

%% intervaldb_info(+IDB) is det
% prints out information on an IDB

%% intervaldb_addone(+IDB,+INDEX,+X:int,+Y:int) is det
% adds to an already instantiated IDB a span intersection span(X,Y)
% at position INDEX
% TODO: should this be called intervaldb_put? add implies adding
% an element to a set or list. put is conventional for positional/array

%% nclist_build(+IDB,-NCL) is det
% creates a nested containment list from an IDB
% TODO: rename intervaldb_to_nclist/2

%% nclist_intersection(+NCL,+X:int,+Y:int,-MDB) is det
% finds spans intersecting range X-Y in NCL
% and unifies results as an IDB in MDB
% which can then be used again to build another NCL

%% nclist_intersection2(+NCL,+X:int,+Y:int,-SLIST) is det
% finds intersections of X and Y in NCL
% and unifies results as SLIST a list of span/3 terms
% which can then be used again to build another NCL

%% intervaldb_intersection(+IDB,+X:int,+Y:int,-MDB) is det
% finds intersection of X and Y in an IDB
% and unifies results as an IDB in MDB
% which can then be used again to build another NCL

%% intervaldb_intersection2(+IDB,+X:int,+Y:int,-MDB) is det
% finds intersection of X and Y in an IDB or NCL
% and unifies results as an IDB in MDB
% which can then be used again to build another NCL

%% intervaldb_match(+MDB,?X:int,?Y:int) is nondet.
% succeeds if X-Y is an interval in MDB (which is an IDB)

%% intervaldb_match(+IDB,+RX:int,+RY:int,?X:int,?Y:int) is nondet.
% RX and RY are the intersections to be matched with the IDB
% X and Y are the intersections matched

%% intervaldb_match(+IDB,?X:int,?Y:int,?ID:int) is nondet.
% X and Y are the intersections matched and an ID

%% intervaldb_match(+IDB,+SDB,?X:int,?Y:int,?ID:char*,?S:int,?E:int) is nondet.
% X and Y are the intersections matched, ID is name of span with 
% S and E being target intervals for the ID

%% intervaldb_addall(+IDB,+LSPAN) is det
% adds to an already instantiated IDB a list of
% span intersections span(X,Y)

%% intervaldb_addone(+IDB,+X:int,+Y:int) is det
% adds to an already instantiated IDB a span intersection span(X,Y)
% non-det, adds to the next free interval, done in prolog
intervaldb_addone(IDB,S,E):-intervaldb_addall(IDB,[span(S,E)]).

%% intervaldb_free(+IDB) is det
% NECESSARY to free up memory from any IDB, ie IDB, NCL or MDB

%% read_intervals(+FILE:char*,+N:int,-IDB) is det
% reads intervals from a file FILE of length N into IDB

%% spandb_make(-SDB) is det
% creates the initial datatype from which to build an SDB

%% spandb_addone(+SDB,+S:int,+E:int,+ID:int,+ID_S:int,+ID_E:int) is det
% adds a span interval (S,E,ID,ID_S,ID_E) to an already instantiated SDB

%% spandb_print(+SDB) is det
% prints out the SDB

%% spandb_to_intervaldb(+SDB,-IDB) is det
% converts an SDB into an IDB

%% spandb_free(+SDB) is det
% NECESSARY to free up memory from an SDB

%% spandb2_make(-SDB) is det
% creates the initial datatype from which to build an SDB

%% spandb2_addone(+SDB,+S:int,+E:int,+ID:char*,+ID_S:int,+ID_E:int) is det
% adds a span interval (S,E,ID,ID_S,ID_E) to an already instantiated SDB

%% spandb2_print(+SDB) is det
% prints out the SDB

%% spandb2_to_intervaldb(+SDB,-IDB) is det
% converts an SDB into an IDB

%% spandb2_free(+SDB) is det
% NECESSARY to free up memory from an SDB




%% TODO: benchmarks. generate testdb, then perform timed retrievals
% see time_goal/2

% TODO: change prolog interval list representation to something more structured.
% E.g. Intervals = [ span(10,30), span(20,29), ...]
% or   Intervals = [ 10-30, 20-29, ...]

spandb_size(Size) :-
       nb_setval(span_ix,0),
       span(_,_,_),
       nb_getval(span_ix,Ix),
       Ix2 is Ix+1,
       nb_setval(span_ix,Ix2),
       fail.
spandb_size(Size) :-
       nb_getval(span_ix,Size).

%% intervaldb_make(-IDB:intervaldb) is det
% generates an IDB using span/3
intervaldb_make(IDB) :-
    count_span(Size),
    intervaldb_make(Size,IDB),
    set_x(0),
    forall(span(_,S,E),
           (   get_x(I),intervaldb_addone(IDB,I,S,E),inc_x) ).

%% nclist_make(-NCL:nclist) is det
% generates a nested containment list using span/3.
% this is a simple composition of intervaldb_make/2 and nclist_build/2
nclist_make(NCL) :-
        intervaldb_make(IDB),
        nclist_build(IDB,NCL).

%% nclist_intersection_span(+NCL,+ContainerS:int,+ContainerE:int,?MatchS:int,?MatchE:int)
% composition of nclist_intersection/4 and intervaldb_match/3
% this is convenient for scenarios in which we do not need to perform subsequent operations on the matchdb
% Example:
% ==
% load_file('testdb.qlf'),nclist_make(NCL),nclist_intersection_span(NCL,1000,5000,S,E)
% ==
nclist_intersection_span(NCL,ContainerS,ContainerE,MatchS,MatchE) :-
        nclist_intersection(NCL,ContainerS,ContainerE,MDB),
        intervaldb_match(MDB,MatchS,MatchE).

spandb_size(Size) :-
        nb_setval(span_ix,0),
        span(_,_,_),
        nb_getval(span_ix,Ix),
        Ix2 is Ix+1,
        nb_setval(span_ix,Ix2),
        fail.
spandb_size(Size) :-
        nb_getval(span_ix,Size).

% works for Size < ~1m
spandb_size_x1(Size) :-
        aggregate(count,span(_,_,_),Size).
% works for Size < ~1m
spandb_size_x2(Size) :-
        findall(S-E,span(_,S,E),L),
        length(L,Size).



% helpful predicates in building span lists and databases

build_span_list([],[],[]).

build_span_list([H1|T1],[H2|T2],[span(H1,H2)|L]) :-
    build_span_list(T1,T2,L).

build_span_list(L) :-
    findall(X,span(_,X,_),Xs),
    findall(Y,span(_,_,Y),Ys),
    build_span_list(Xs,Ys,L).

count_span(N):-
    set_x(0),
    forall(span(_,_,_),
               inc_x),
    get_x(N).

make_span_db(IDB):-
    count_span(N),
    intervaldb_make(N,IDB),
    set_x(0),
    forall(span(_,S,E),
                intervaldb_addone2(IDB,S,E)).
%               (get_x(I),intervaldb_addone(IDB,I,S,E),inc_x) ).

spandb_to_intervaldb(IDB):-
    spandb_make(SDB),
    forall(span(ID,S,E), spandb_addone(SDB,S,E,ID,0,0)),
    spandb_to_intervaldb(SDB,IDB),
    spandb_free(SDB).

make_span_list(SL1,IDs,SL2):-
    sort(SL1,SL),
    make_span_list(SL,IDs,0,SL2).

make_span_list([],_,_,[]).

make_span_list([span(X,S,E)|T1],[H2|T2],X,[span(H2,S,E)|T3]):-
    Y is X+1,
%    format('Match ~w ~w ~w ~w~n',[X,H2,S,E]),
    !,
    make_span_list(T1,T2,Y,T3).

make_span_list([span(Z,S,E)|T1],[_|T2],X,L3):-
    Z =\= X,
    Y is X+1,
    !,
    make_span_list([span(Z,S,E)|T1],T2,Y,L3).

/** <module> wrapper for Nested Containment Lists

  ---+ Synopsis

==
:- use_module(bio(intervaldb)).

% 
demo:-
  nl.
  

==

---+ Details

An SWI-Prolog wrapper for the PyGR Nested Containment List (NCL) library.

NCLs allow efficient retrieval of intervals within a region of interest

---++ Data Structures

 NB a nested containment list (NCL or nclist) is a *IntervalDB (IDB)
 which has been built using nclist_build/2 so IDB, NCL and MDB are all
 the same datatype all IDBs must be freed explicitly using
 intervaldb_free/1 the overhead in building an NCL from an IDB is
 substantial so an IDB should be used if there are <100 intersections
 to be made a spandb (SDB) is a linked list of
 span(int,int,int,int,int) a spandb2 (SDB) is a linked list of
 span(int,int,char*,int,int) all SDBs must be freed explicitly using
 spandb_free/1 or spandb2_free/1

---+++ Interval DB

TODO DOCUMENTATION

---+++ NCL

TODO DOCUMENTATION

---+++ MDB

TODO DOCUMENTATION

An NCL that...



---+ Installation

First you need to build the intervaldb library

TODO

---+ TODO

* docs
* tests
* integration with range.pro and seqfeature_db.pro -- e.g. feature_intersects/2
 

---+ Additional Information

This module is part of blip. For more details, see http://www.blipkit.org

@author  Stephen W Veitch

*/

