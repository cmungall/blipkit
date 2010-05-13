:- use_module(intervaldb).

/*

  To run these tests, start up swipl and execute the following:

  ==
  ['intervaldb.plt'].
  set_prolog_flag(verbose,normal),
  set_test_options([silent(false)]),
  run_tests.
  ==

  or this:
  ==
  swipl -g "['intervaldb.plt'],set_prolog_flag(verbose,normal),set_test_options([silent(false)]), run_tests."
  ==

  */

% Chris stuff

%% solutions(+Template,+Goal,-Set)
%   deterministic setof (will not fail). Goal is existentially quantified
solutions(X,Goal,Xs):-
        (   setof(X,Goal^Goal,Xs)
        ->  true
        ;   Xs=[]).

time_goal(Goal,Time):-
       statistics(cputime,T1),
       Goal,
       statistics(cputime,T2),
       Time is T2-T1.

%random_pos(P) :- random(X),P is floor(X * 3000000000).
%random_pos(P) :- random(X),P is floor(X * 3000).
%big_random_pos(P) :- random(X),P is floor(X * 300000).
random_pos(P) :- P is random(3000).
big_random_pos(P) :- P is random(30000).

write_random_span_database :-
        write_random_span_database(2000000).

write_random_span_database(N):-
        tell('testdb1.pl'),
        forall(between(1,N,ID),
               generate_random_span(ID)),
        told.

create_name(ID,Name):-
        string_to_atom(IDS,ID),
        string_concat('name',IDS,Name).

generate_random_span(ID):-
        big_random_pos(X),
        random_pos(Z),
        Y is X+Z,
%        big_random_pos(Y),
%        S is min(X,Y),
%        E is max(X,Y),
%        format('span(~w,~w,~w).~n',[ID,S,E]).
%        create_name(ID,Name),
        format('span(~w,~w,~w).~n',[ID,X,Y]).

write_random_span_database2(N):-
        tell('testdb2.pl'),
        forall(between(1,N,ID),
               generate_random_span2(ID)),
        told.

generate_random_span2(ID):-
        big_random_pos(X),
        random_pos(Z),
        Y is X+Z,
        create_name(ID,Name),
        format('span(~w,~w,~w).~n',[Name,X,Y]).

write_random_interval_database(N):-
        tell('data2'),
        forall(between(1,N),
               generate_random_interval),
        told.

generate_random_interval:-
        random_pos(X),
        random_pos(Y),
        S is min(X,Y),
        E is max(X,Y),
        format('~w,~w,0,0,0~n',[S,E]).

% this test is on a teensy dataset
% assumes file data1 in directory

:- begin_tests(small_dataset_test,[]).



% these are all basic tests of intervaldb_match,
% also testing read_intervals/3 and intervaldb_addall/2
test(idb1) :-
    test_idb1a,
    test_idb1b,
    test_idb1c,
    format('test_idb1 ok~n').

% testing intervaldb_match/3 with a small set and nclist_intersection
test_idb1a:-
    read_intervals(data1,6,IDB),
    intervaldb_print(IDB),
    intervaldb_addall(IDB,[span(10,20),span(20,30),span(30,40)]),
    intervaldb_print(IDB),
    nclist_build(IDB,NCL),
    nclist_intersection(NCL,12,14,MDB),
    forall(intervaldb_match(MDB,S,E),
           format('intersecting interval:~w-~w~n',[S,E])),
    intervaldb_info(NCL),
    intervaldb_free(NCL),
    intervaldb_free(MDB),
    intervaldb_free(IDB).

% testing intervaldb_match/5 with a small set
test_idb1b:-
    read_intervals(data1,6,IDB),
    intervaldb_print(IDB),
    intervaldb_addall(IDB,[span(10,20),span(20,30),span(30,40)]),
    intervaldb_print(IDB),
    forall(intervaldb_match(IDB,12,14,S,E),
        format('intersecting interval:~w-~w~n',[S,E])),
    intervaldb_info(IDB),
    intervaldb_free(IDB).

% testing intervaldb_match/4 with a small set
test_idb1c:-
    read_intervals(data1,6,IDB),
    intervaldb_print(IDB),
    intervaldb_addall(IDB,[span(10,20),span(20,30),span(30,40)]),
    intervaldb_print(IDB),
    nclist_build(IDB,NCL),
    nclist_intersection(NCL,12,14,MDB),
    forall(intervaldb_match(MDB,S,E,ID),
        format('intersecting interval:~w-~w~n',[S,E])),
    intervaldb_info(IDB),
    intervaldb_free(IDB).


:- end_tests(small_dataset_test).


:- begin_tests(big_dataset_test,[setup(init_database)]).


init_database :-
    write_random_span_database(1000000),
    write_random_span_database2(1000000).

% these are all basic tests of intervaldb_match,
test(idb2):-
% first on large span(ID:int,S:int,E:int) datatbases
    [testdb1],
    test_idb2a,
    test_idb2b,
    test_idb2c,
    test_idb2d,
    test_idb2e,
    test_idb2f,
    test_idb2g,
% then on large span(ID:char*,S:int,E:int) datatbases
    [testdb2],
    test_idb2h,
    test_idb2i,
    format('test_idb2 ok~n').

% testing intervaldb_match/3 with a large set and an NCL
test_idb2a:-
    spandb_to_intervaldb(IDB),
    intervaldb_info(IDB),
    nclist_build(IDB,NCL),
    nclist_intersection(NCL,10000,10100,MDB),
    forall(intervaldb_match(MDB,S,E),
        format('')),  % TODO -- what??
    intervaldb_info(MDB),
    intervaldb_free(NCL),
    intervaldb_free(MDB),
    intervaldb_free(IDB).

% testing intervaldb_match/3 with a large set and an IDB
test_idb2b:-
    spandb_to_intervaldb(IDB),
    intervaldb_info(IDB),
    intervaldb_intersection(IDB,10000,10100,MDB),
    forall(intervaldb_match(MDB,S,E),
%        format('intersecting interval:~w-~w~n',[S,E])),
        format('')),
    intervaldb_info(MDB),
    intervaldb_free(MDB),
    intervaldb_free(IDB).

% testing intervaldb_match/5 with a large set and an NCL
test_idb2c:-
    spandb_to_intervaldb(IDB),
    intervaldb_info(IDB),
    nclist_build(IDB,NCL),
    forall(intervaldb_match(NCL,10000,10100,S,E),
        format('')),
    intervaldb_free(NCL),
    intervaldb_free(IDB).

% testing intervaldb_match/5 with a large set and an IDB
test_idb2d:-
    spandb_to_intervaldb(IDB),
    intervaldb_info(IDB),
    forall(intervaldb_match(IDB,10000,10100,S,E),
        format('')),
    intervaldb_free(IDB).

% testing intervaldb_match/4 with a large set and an NCL
test_idb2e:-
    spandb_to_intervaldb(IDB),
    intervaldb_info(IDB),
    nclist_build(IDB,NCL),
    nclist_intersection(NCL,10000,10100,MDB),
    forall(intervaldb_match(MDB,S,E),
        format('')),
    intervaldb_info(MDB),
    intervaldb_free(MDB),
    intervaldb_free(IDB).

% testing intervaldb_match/4 with a large set and an IDB
test_idb2f:-
    spandb_to_intervaldb(IDB),
    intervaldb_info(IDB),
    intervaldb_intersection(IDB,10000,10100,MDB),
    forall(intervaldb_match(IDB,ID,S,E),
        format('')),
    intervaldb_free(IDB).

% testing intervaldb_intersection2 with a large set and an NCL
test_idb2g:-
    spandb_to_intervaldb(IDB),
    intervaldb_info(IDB),
    nclist_build(IDB,NCL),
    intervaldb_intersection2(NCL,10000,10100,MDB),
    forall(intervaldb_match(MDB,S,E),
        format('')),
    intervaldb_info(MDB),
    intervaldb_free(NCL),
    intervaldb_free(MDB),
    intervaldb_free(IDB).

% testing intervaldb_match/7 with a large set and an NCL (uses testdb2)
test_idb2h:-
    spandb2_make(SDB),
    forall(span(ID,X,Y), spandb2_addone(SDB,X,Y,ID,0,0)),
    spandb2_to_intervaldb(SDB,IDB),
    intervaldb_info(IDB),
    nclist_build(IDB,NCL),
    nclist_intersection(NCL,10000,10100,MDB),
    forall(intervaldb_match(MDB,SDB,S,E,ID,ID_S,ID_E),
        format('')),
    intervaldb_info(NCL),
    spandb2_free(SDB),
    intervaldb_free(MDB),
    intervaldb_free(NCL),
    intervaldb_free(IDB).

% testing intervaldb_intersection2 with a large set and an IDB
test_idb2i:-
    spandb2_make(SDB),
    forall(span(ID,X,Y), spandb2_addone(SDB,X,Y,ID,0,0)),
    spandb2_to_intervaldb(SDB,IDB),
    intervaldb_info(IDB),
    intervaldb_intersection2(IDB,10000,10100,MDB),
    forall(intervaldb_match(MDB,SDB,S,E,_,_,_),
        format('')),
    intervaldb_info(MDB),
    intervaldb_free(MDB),
    intervaldb_free(IDB).


% these are a set of basic tests on large span DBs
% they check count, nclist_build, intersection and match times

test(idb3) :-
% first on large span(ID:int,S:int,E:int) datatbases
    [testdb1],
    test_idb3a,
    test_idb3b,
    test_idb3c,
    test_idb3d,
    test_idb3e,
% then on large span(ID:char*,S:int,E:int) datatbases
    [testdb2],
    test_idb3f,
    format('test_idb3 ok~n').


% tests standard intervaldb_make with intervaldb_intersection
test_idb3a:-
    time_goal(count_span(N),T0),
    intervaldb_make(N,X),
    set_x(0),
    time_goal(forall(span(_,S,E),
               (get_x(I),intervaldb_addone(X,I,S,E),inc_x) ),T1),
    intervaldb_info(X),
    T2 is 0,
%    time_goal(intervaldb_intersection(X,10000,10100,MDB),T3),
    time_goal(intervaldb_intersection2(X,10000,10100,MDB),T3),
    time_goal(forall(intervaldb_match(MDB,_,_),true),T4),
    intervaldb_info(MDB),
    intervaldb_free(X),
    intervaldb_free(MDB),
    format('3a For:~w Count:~w Add:~w Build:~w Intersect:~w Match:~w~n',[N,T0,T1,T2,T3,T4]).

% tests standard intervaldb_make with nclist_intersection
test_idb3b:-
    time_goal(count_span(N),T0),
    intervaldb_make(N,X),
    set_x(0),
    time_goal(forall(span(_,S,E),
               (get_x(I),intervaldb_addone(X,I,S,E),inc_x) ),T1),
    intervaldb_info(X),
    time_goal(nclist_build(X,NCL),T2),
    time_goal(nclist_intersection(NCL,10000,10100,MDB),T3),
    time_goal(forall(intervaldb_match(MDB,_,_),true),T4),
    intervaldb_info(MDB),
    intervaldb_free(X),
    intervaldb_free(MDB),
    intervaldb_free(NCL),
    format('3b For:~w Count:~w Add:~w Build:~w Intersect:~w Match:~w~n',[N,T0,T1,T2,T3,T4]).

% tests standard intervaldb_make with nclist_intersection
test_idb3c:-
    time_goal(count_span(N),T0),
    intervaldb_make(N,X),
    set_x(0),
    time_goal(forall(span(_,S,E),
               (get_x(I),intervaldb_addone(X,I,S,E),inc_x) ),T1),
    intervaldb_info(X),
    time_goal(nclist_build(X,NCL),T2),
    time_goal(nclist_intersection2(NCL,10000,10100,_),T3),
    T4 = 0,
    intervaldb_free(X),
    intervaldb_free(NCL),
    format('3c For:~w Count:~w Add:~w Build:~w Intersect:~w Match:~w~n',[N,T0,T1,T2,T3,T4]).

% tests spandb with intervaldb_intersection
test_idb3d:-
    T0 is 0,
    time_goal(spandb_to_intervaldb(X),T1),
    intervaldb_info(X),
    T2 is 0,
    time_goal(intervaldb_intersection(X,10000,10100,MDB),T3),
    time_goal(forall(intervaldb_match(MDB,_,_),true),T4),
    intervaldb_info(MDB),
    intervaldb_free(X),
    intervaldb_free(MDB),
    format('3d For:? Count:~w Add:~w Build:~w Intersect:~w Match:~w~n',[T0,T1,T2,T3,T4]).

% tests spandb with intervaldb_intersection2
test_idb3e:-
    T0 is 0,
%    time_goal(spandb_to_intervaldb(X),T1),
    time_goal((spandb_make(SDB),
              forall(span(ID,S,E), 
                  spandb_addone(SDB,S,E,ID,0,0)),
              spandb_to_intervaldb(SDB,X)),T1),
    intervaldb_info(X),
    T2 is 0,
    time_goal(intervaldb_intersection2(X,10000,10100,MDB),T3),
    time_goal(forall(intervaldb_match(MDB,_,_),true),T4),
    intervaldb_info(MDB),
    intervaldb_free(X),
    intervaldb_free(MDB),
    format('3e For:? Count:~w Add:~w Build:~w Intersect:~w Match:~w~n',[T0,T1,T2,T3,T4]).

% tests spandb2 with intervaldb_intersection2
test_idb3f:-
    T0 is 0,
    time_goal((spandb2_make(SDB),
              forall(span(ID,S,E), 
                  spandb2_addone(SDB,S,E,ID,0,0)),
              spandb2_to_intervaldb(SDB,X)),T1),
    intervaldb_info(X),
    T2 is 0,
    time_goal(intervaldb_intersection2(X,10000,10100,MDB),T3),
    time_goal(forall(intervaldb_match(MDB,SDB,_,_,_,_,_),true),T4),
    intervaldb_info(MDB),
    intervaldb_free(X),
    spandb2_free(SDB),
    intervaldb_free(MDB),
    format('3f For:? Count:~w Add:~w Build:~w Intersect:~w Match:~w~n',[T0,T1,T2,T3,T4]).

% tests spandb2 with nclist_intersection
test_idb3g:-
    T0 is 0,
    time_goal((spandb2_make(SDB),
              forall(span(ID,S,E), 
                  spandb2_addone(SDB,S,E,ID,0,0)),
              spandb2_to_intervaldb(SDB,X)),T1),
    intervaldb_info(X),
    time_goal(nclist_build(X,NCL),T2),
    time_goal(nclist_intersection(NCL,10000,10100,MDB),T3),
    time_goal(forall(intervaldb_match(MDB,SDB,_,_,_,_,_),true),T4),
    intervaldb_info(MDB),
    intervaldb_free(X),
    spandb2_free(SDB),
    intervaldb_free(MDB),
    format('3f For:? Count:~w Add:~w Build:~w Intersect:~w Match:~w~n',[T0,T1,T2,T3,T4]).

% tests spandb2 with intervaldb_intersection2 and spandb2_to_intervaldb2
test_idb3h:-
    T0 is 0,
    time_goal((spandb2_make(SDB),
              forall(span(ID,S,E), 
                  spandb2_addone(SDB,S,E,ID,0,0)),
              spandb2_to_intervaldb(SDB,X,NDB)),T1),
    intervaldb_info(X),
    T2 is 0,
    time_goal(intervaldb_intersection2(X,10000,10100,MDB),T3),
    time_goal(forall(intervaldb_match2(MDB,NDB,_,_,_,_,_),true),T4),
    intervaldb_info(MDB),
    intervaldb_free(X),
    spandb2_free(SDB),
    intervaldb_free(MDB),
    format('3h For:? Count:~w Add:~w Build:~w Intersect:~w Match:~w~n',[T0,T1,T2,T3,T4]).


% similar to above using a small hand made data set

% small hand made data set test of nclist_intersection using an nclist
test(intervaldb4) :-
    N is 6,
    time_goal(read_intervals(data1,N,X),T1),
    set_x(0),
    format('N is ~w~n',[N]),
    intervaldb_info(X),
    time_goal(nclist_build(X,X2),T2),
    intervaldb_info(X2),
    time_goal(nclist_intersection(X2,10,12,MDB),T3),
    intervaldb_info(MDB),
    format('For:~w Add:~w Build:~w Intersect:~w~n',[N,T1,T2,T3]).

% small hand made data set test of intervaldb_intersection using O(N)
test(intervaldb4a) :-
    N is 6,
    time_goal(read_intervals(data1,N,X),T1),
    format('N is ~w~n',[N]),
    intervaldb_info(X),
    T2 is 0,
    time_goal(intervaldb_intersection(X,10,12,MDB),T3),
    intervaldb_info(MDB),
    format('For:~w Add:~w Build:~w Intersect:~w~n',[N,T1,T2,T3]).

% small hand made data set test of nclist_intersection2 using an nclist
test_intervaldb4b(MDB):-
    N is 6,
    time_goal(read_intervals(data1,N,X),T1),
    format('N is ~w~n',[N]),
    intervaldb_info(X),
    time_goal(nclist_build(X,X2),T2),
    intervaldb_info(X2),
    time_goal(nclist_intersection2(X2,10,12,MDB),T3),
    format('For:~w Add:~w Build:~w Intersect:~w~n',[N,T1,T2,T3]).


% tests to show memory problems

% this first one should break swipl after 3 or so uses
test_intervaldb5 :-
    intervaldb_make(5000000,X),
    format('1: 5000000'),
    intervaldb_info(X),
    intervaldb_make(5000000,X2),
    format('2: 5000000'),
    intervaldb_info(X2),
    intervaldb_make(5000000,X3),
    format('3: 5000000'),
    intervaldb_info(X3),
    intervaldb_make(5000000,X4),
    format('4: 5000000'),
    intervaldb_info(X4),
    intervaldb_make(5000000,X5),
    format('5: 5000000'),
    intervaldb_info(X5),
    intervaldb_make(5000000,X6),
    format('6: 5000000'),
    intervaldb_info(X6),
    intervaldb_make(5000000,X7),
    format('7: 5000000'),
    intervaldb_info(X7),
    intervaldb_make(5000000,X8),
    intervaldb_info(X8),
    format('8: 5000000').

% this one should never break as all memory is freed
test_intervaldb5a:-
    intervaldb_make(5000000,X),
    format('1: 5000000'),
    intervaldb_free(X),
    intervaldb_make(5000000,X2),
    format('2: 5000000'),
    intervaldb_free(X2),
    intervaldb_make(5000000,X3),
    format('3: 5000000'),
    intervaldb_free(X3),
    intervaldb_make(5000000,X4),
    format('4: 5000000'),
    intervaldb_free(X4),
    intervaldb_make(5000000,X5),
    format('5: 5000000'),
    intervaldb_free(X5),
    intervaldb_make(5000000,X6),
    format('6: 5000000'),
    intervaldb_free(X6),
    intervaldb_make(5000000,X7),
    format('7: 5000000'),
    intervaldb_free(X7),
    intervaldb_make(5000000,X8),
    format('8: 5000000'),
    intervaldb_free(X8).

% test to show intervaldb_free/1 works on NCL, IDB and MDB 
test_intervaldb5b:-
    N is 6,
    read_intervals(data1,N,X),
    intervaldb_info(X),
    format('1: 5000000\n'),
    nclist_build(X,X2),
    nclist_intersection(X2,10,12,MDB),
    format('2: before free\n'),
    intervaldb_free(X),
    format('3: after free X\n'),
    intervaldb_free(X2),
    format('4: after free X2\n'),
    intervaldb_free(MDB),
    format('5: after free MDB\n'),
    intervaldb_info(X).


% tests out intervaldb_match, non-det predicate on large scale span dbs

% span db must be created using write_span_database(N)and [testdb]
test(intervaldb7):-
    count_span(N),
    intervaldb_make(N,X),
    set_x(0),
    time_goal(forall(span(_,S,E),
               (get_x(I),intervaldb_addone(X,I,S,E),inc_x) ),T1),
    intervaldb_info(X),
    time_goal(nclist_build(X,X2),T2),
    intervaldb_info(X2),
    time_goal(nclist_intersection(X2,100000,100100,MDB),T3),
    format('For:~w Add:~w Build:~w Intersect:~w~n',[N,T1,T2,T3]),
    forall(intervaldb_match(MDB,S,E),
           (format('S:~w E:~w~n',[S,E]),
              nclist_intersection(X2,S,E,MDB2),
              forall(intervaldb_match(MDB2,S2,E2),
                  format('    S:~w E:~w~n',[S2,E2])),
              intervaldb_free(MDB2))),
    format('For:~w Add:~w Build:~w Intersect:~w~n',[N,T1,T2,T3]),
    intervaldb_free(X),
    intervaldb_free(X2),
    intervaldb_free(MDB),
    format('All memory freed~n').

% tests out intervaldb_match, non-det predicate on large scale span dbs
% span db must be created using write_random_span_database(N)and [testdb]
test(intervaldb7a):-
    count_span(N),
    intervaldb_make(N,X),
    set_x(0),
    time_goal(forall(span(_,S,E),
               (get_x(I),intervaldb_addone(X,I,S,E),inc_x) ),T1),
    intervaldb_info(X),
    T2 is 0,
    time_goal(intervaldb_intersection(X,100000,100100,MDB),T3),
    forall(intervaldb_match(MDB,S,E),
           (format('S:~w E:~w~n',[S,E]),
              intervaldb_intersection(X,S,E,MDB2),
              forall(intervaldb_match(MDB2,S2,E2),
                  format('    S:~w E:~w~n',[S2,E2])),
              intervaldb_free(MDB2))),
    format('For:~w Add:~w Build:~w Intersect:~w~n',[N,T1,T2,T3]),
    intervaldb_free(X),
    intervaldb_free(MDB),
    format('All memory freed~n').

% tests out intervaldb_match, non-det predicate 
% same as test_intervaldb7 but works on a small hand built file of data
test(intervaldb7b):-
    N is 6,
    time_goal(read_intervals(data1,N,X),T1),
    intervaldb_info(X),
    time_goal(nclist_build(X,X2),T2),
    intervaldb_info(X2),
    time_goal(nclist_intersection(X2,10,12,MDB),T3),
    format('For:~w Add:~w Build:~w Intersect:~w~n',[N,T1,T2,T3]),
    forall(intervaldb_match(MDB,S,E),
           (format('S:~w E:~w~n',[S,E]),
              nclist_intersection(X2,S,E,MDB2),
              forall(intervaldb_match(MDB2,S2,E2),
                  format('    S:~w E:~w~n',[S2,E2])), 
              intervaldb_free(MDB2)) ),
    format('For:~w Add:~w Build:~w Intersect:~w~n',[N,T1,T2,T3]),
    intervaldb_free(X),
    intervaldb_free(X2),
    intervaldb_free(MDB),
    format('All memory freed~n').
 
% tests out intervaldb_match, non-det predicate
% same as test_intervaldb7 but works on a small hand built file of data
% and uses intervaldb_intersection/4, to test O(n).
test(intervaldb7c) :-
    N is 6,
    time_goal(read_intervals(data1,N,X),T1),
    intervaldb_info(X),
    T2 is 0,
    time_goal(intervaldb_intersection(X,10,12,MDB),T3),
    format('For:~w Add:~w Build:~w Intersect:~w~n',[N,T1,T2,T3]),
    forall(intervaldb_match(MDB,S,E),
           (format('S:~w E:~w~n',[S,E]),
              intervaldb_intersection(X,S,E,MDB2),
              forall(intervaldb_match(MDB2,S2,E2),
                  format('    S:~w E:~w~n',[S2,E2])),
              intervaldb_free(MDB2)) ),
    format('For:~w Add:~w Build:~w Intersect:~w~n',[N,T1,T2,T3]),
    intervaldb_free(X),
    intervaldb_free(MDB),
    format('All memory freed~n').

 
% tests out intervaldb_match, non-det predicate 

% same as test_intervaldb7 but works on 2 small hand built files of data
% and uses intervaldb_intersection/4, to test NCL.
% uses two dbs the first to find matches to intersect with the second
% TODO: move to small_dataset_set
test(intervaldb8) :-
    N is 6,
    read_intervals(data1,N,XIDB),
    intervaldb_info(XIDB),
    nclist_build(XIDB,XNCL),
    nclist_intersection(XNCL,10,12,XMDB),
    read_intervals(data2,N,YIDB),
    intervaldb_info(YIDB),
    nclist_build(YIDB,YNCL),
    forall(intervaldb_match(XMDB,S,E),
           (format('S:~w E:~w~n',[S,E]),
              nclist_intersection(YNCL,10,12,YMDB),
              forall(intervaldb_match(YMDB,S2,E2),
                  format('    S:~w E:~w~n',[S2,E2])),
              intervaldb_free(YMDB)) ),
    intervaldb_free(XIDB),
    format('XIDB freed~n'),
    intervaldb_free(YIDB),
    format('YIDB freed~n'),
    intervaldb_free(XNCL),
    format('XNCL freed~n'),
    intervaldb_free(YNCL),
    format('YNCL freed~n'),
    intervaldb_free(XMDB),
    format('XMDB freed~n'),
    format('All memory freed~n').
 

% same as test_intervaldb8 but works on 2 small hand built files of data
% and uses intervaldb_intersection/4, to test O(N) intersection.
% uses two dbs the first to find matches to intersect with the second
% TODO: move to small_dataset_set
test(intervaldb8a) :-
    N is 6,
    read_intervals(data1,N,XIDB),
    intervaldb_info(XIDB),
    intervaldb_intersection(XIDB,10,12,XMDB),
    read_intervals(data2,N,YIDB),
    intervaldb_info(YIDB),
    forall(intervaldb_match(XMDB,S,E),
           (format('S:~w E:~w~n',[S,E]),
              intervaldb_intersection(YIDB,10,12,YMDB),
              forall(intervaldb_match(YMDB,S2,E2),
                  format('    S:~w E:~w~n',[S2,E2])),
              intervaldb_free(YMDB)) ),
    intervaldb_free(XIDB),
    intervaldb_free(YIDB),
    intervaldb_free(XMDB),
    format('All memory freed~n').


% same as test_intervaldb8 but works on 2 small hand built files of data
% and uses intervaldb_intersection/4, to test NCL.
% uses two dbs the first to find matches to intersect with the second
% and constructs the IntervalDB using a spandb
test(intervaldb8b) :-
    [spandb1],
    spandb_to_intervaldb(XIDB),
    intervaldb_info(XIDB),
    nclist_build(XIDB,XNCL),
    intervaldb_intersection(XNCL,10,12,XMDB),
    [spandb2],
    spandb_to_intervaldb(YIDB),
    intervaldb_info(YIDB),
    nclist_build(YIDB,YNCL),
    forall(intervaldb_match(XMDB,S,E),
           (format('S:~w E:~w~n',[S,E]),
              nclist_intersection(YNCL,10,12,YMDB),
              forall(intervaldb_match(YMDB,S2,E2),
                  format('    S:~w E:~w~n',[S2,E2])),
              intervaldb_free(YMDB)) ),
    intervaldb_free(XIDB),
    intervaldb_free(YIDB),
    intervaldb_free(XNCL),
    intervaldb_free(YNCL),
    intervaldb_free(XMDB),
    format('All memory freed~n').
 

% these tests compare relative speeds of creating and
% populating an IntervalDB 

% first using chris method
test_intervaldb9:-
    time_goal(intervaldb_make(IDB),T),
    intervaldb_info(IDB),
    format('intervaldb_make t:~w~n',[T]),
    intervaldb_free(IDB).

% tests out same with spandb 
test_intervaldb9a:-
    time_goal(spandb_to_intervaldb(IDB),T),
    intervaldb_info(IDB),
    format('spandb t:~w~n',[T]),
    intervaldb_free(IDB).


% a series of tests on spandb2 which uses span(char*, int, int, int int)

% uses a small set of data and intervaldb_intersection/4 to get an MDB
test_intervaldb10:-
    [span_named],
    spandb2_make(SDB),
    findall(ID, span(ID,_,_), IDs),
    forall(span(I,S,E), spandb2_addone(SDB,S,E,I,0,0)),
    spandb2_to_intervaldb(SDB,IDB),
    spandb2_print(SDB),
    intervaldb_print(IDB),
    intervaldb_intersection(IDB,10,12,MDB),
    intervaldb_print(MDB),
    forall(intervaldb_match(MDB,S,E,ID1),
        (nth0(ID1,IDs,ID2), format('idb: ~w ~w ~w~n',[S,E,ID2]))),
    spandb2_free(SDB),
    intervaldb_free(MDB),
    intervaldb_free(IDB).
    
% uses a small set of data and nclist_intersection to get a span list
test_intervaldb10a(SL,SPANS):-
    [span_named],
    spandb2_make(SDB),
    findall(ID, span(ID,_,_), IDs),
    forall(span(I,S,E), spandb2_addone(SDB,S,E,I,0,0)),
    spandb2_to_intervaldb(SDB,IDB),
    intervaldb_print(IDB),
    nclist_build(IDB,NCL),
    nclist_intersection2(NCL,10,12,SPANS),
    make_span_list(SPANS,IDs,SL),
    spandb2_free(SDB),
    intervaldb_free(NCL),
    intervaldb_free(IDB).
    
% uses a small set of data and intervaldb_intersection to get an MDB
% which is matched with intervaldb_match/7
test_intervaldb10b:-
    [span_named],
    spandb2_make(SDB),
    forall(span(I,S,E), spandb2_addone(SDB,S,E,I,0,0)),
    spandb2_to_intervaldb(SDB,IDB),
    spandb2_print(SDB),
    intervaldb_print(IDB),
    intervaldb_intersection(IDB,10,12,MDB),
    intervaldb_print(MDB),
    forall(intervaldb_match(MDB,SDB,S,E,ID,ID_S,ID_E), format('spandb ~w ~w ~w~w ~w~n',[ID,S,E,ID_S,ID_E])),
    spandb2_free(SDB),
    intervaldb_free(MDB),
    intervaldb_free(IDB).
    
% uses a large set of data and nclist_intersection to get a span list
test_intervaldb10c(SL):-
    spandb2_make(SDB),
    findall(ID, span(ID,_,_), IDs),
    forall(span(I,S,E), spandb2_addone(SDB,S,E,I,0,0)),
    spandb2_to_intervaldb(SDB,IDB),
    intervaldb_info(IDB),
    nclist_build(IDB,NCL),
    nclist_intersection2(NCL,10000,10100,SPANS),
    length(SPANS,LEN),
    format('Span list: ~w~n',[LEN]),
    make_span_list(SPANS,IDs,SL),
    spandb2_free(SDB),
    intervaldb_free(NCL),
    intervaldb_free(IDB).
    
% uses a large set of data and intervaldb_intersection to get an MDB
% which is matched with intervaldb_match/7
test_intervaldb10d:-
    spandb2_make(SDB),
    forall(span(I,S,E), spandb2_addone(SDB,S,E,I,0,0)),
    spandb2_to_intervaldb(SDB,IDB),
    intervaldb_info(IDB),
    intervaldb_intersection(IDB,10000,10100,MDB),
    intervaldb_info(MDB),
%    forall(intervaldb_match(MDB,SDB,S,E,ID,ID_S,ID_E), format('spandb ~w ~w ~w ~w ~w~n',[ID,S,E,ID_S,ID_E])),
    forall(intervaldb_match(MDB,SDB,_,_,_,_,_), format('')),
    spandb2_free(SDB),
    intervaldb_free(MDB),
    intervaldb_free(IDB).
    
% uses a large set of data and nclist_intersection to get an MDB
% which is matched with spandb2_match
test_intervaldb10e:-
    spandb2_make(SDB),
    forall(span(I,S,E), spandb2_addone(SDB,S,E,I,0,0)),
    spandb2_to_intervaldb(SDB,IDB),
    intervaldb_info(IDB),
    nclist_build(IDB,NCL),
    nclist_intersection(NCL,10000,10100,MDB),
    intervaldb_info(MDB),
    set_x(0),
    forall(intervaldb_match(MDB,SDB,_,_,_,_,_), inc_x),
    get_x(Y),
    format('Spans matched ~w~n',[Y]),
    spandb2_free(SDB),
    intervaldb_free(MDB),
    intervaldb_free(NCL),
    intervaldb_free(IDB).
    
:- end_tests(big_dataset_test).

