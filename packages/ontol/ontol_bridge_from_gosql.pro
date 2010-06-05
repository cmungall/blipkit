/* -*- Mode: Prolog -*- */


:- module(ontol_bridge_from_gosql,
          [
          ]).

:- multifile ontol_db:goal_expansion/2.

:- use_module(bio(ontol_db),[]).
:- use_module(bio(rdb_util)).


getrdb(Rdb):-
        nb_getval(rdb,Rdb).
setrdb(Rdb):-
        nb_setval(rdb,Rdb).

set_bridge_resource(Resource):-
        rdb_connect(Rdb,Resource),
        setrdb(Rdb).

rdb_util:qmap(class(ID,N),
              sql(cols=['acc'=ID,
                        'name'=N],
                  from=['term'])).
rdb_util:qmap(belongs(ID,N),
              sql(cols=['acc'=ID,
                        'term_type'=N],
                  from=['term'])).
rdb_util:qmap(synonym(ID,T,N),
              sql(cols=['t.acc'=ID,
                        'st.name'=T,
                        's.term_synonym'=N],
                  from=['term AS t INNER JOIN term_synonym AS s ON (t.id=s.term_id) INNER JOIN term AS st ON (s.synonym_type_id=st.id)'])).
rdb_util:qmap(parent(ID,T,PID),
              sql(cols=['t.acc'=ID,
                        'rt.acc'=T,
                        'pt.acc'=PID],
                  from=['term AS t INNER JOIN term2term ON (t.id=term2term.term2_id) INNER JOIN term AS pt ON (pt.id = term2term.term1_id) INNER JOIN term AS rt ON (rt.id=term2term.relationship_type_id)'])).

ontol_db:class(ID,N):-
        getrdb(Rdb),
        rdb_lookup(Rdb,class(ID,N)).
ontol_db:belongs(ID,N):-
        getrdb(Rdb),
        rdb_lookup(Rdb,belongs(ID,N)).

% parentage:
%  in sqldb, parent is extensional, and subclass/restr is intensional
%  in ontol, parent is intensional, and subclass/restr is extensional

% rewrite view 'parent'
ontol_db:goal_expansion(parent(ID,T,PID),ontol_bridge_from_gosql:parent(ID,T,PID)).
parent(ID,T,PID):-
        getrdb(Rdb),
        rdb_lookup(Rdb,parent(ID,T0,PID)),
        (T0 = is_a -> T=subclass; T=T0).

% these are now views on parent
ontol_db:subclass(ID,PID):-
        getrdb(Rdb),
        rdb_lookup(Rdb,parent(ID,is_a,PID)).
ontol_db:restriction(ID,T,PID):-
        getrdb(Rdb),
        rdb_lookup(Rdb,parent(ID,T,PID)),
        T \= is_a.

% rewrite for efficiency
ontol_db:goal_expansion(noparent(ID),ontol_bridge_from_gosql:noparent(ID)).
noparent(ID):-
        getrdb(Rdb),
        rdb_query(Rdb,sql(select=['term.acc'=ID],
                          from=['term LEFT OUTER JOIN term2term ON (term.id=term2_id)'],
                          where=['term2term.term2_id IS NULL'])).

ontol_db:synonym(ID,T,Syn):-
        getrdb(Rdb),
        rdb_lookup(Rdb,synonym(ID,T,Syn)).

% -------------------- TESTS --------------------
% regression tests to ensure behaviour of module is correct;
% lines below here are not required for module functionality

unittest(load(go)=
      (ontol_bridge_from_gosql:set_bridge_resource(go))/[]).


unittest(test(basic,
            [_=load(go)],
            (   ensure_loaded(bio(ontol_db)),
                ensure_loaded(bio(ontol_bridge_from_gosql)),
                class(ID,'molecular_function'),
                setof(T-CID,parent(CID,T,ID),Kids),
                writeln(subnodes=Kids),
                length(Kids,NumKids)),
            (NumKids>0))).

unittest(xxtest(recursive,
            [_=load(go)],
            (   ensure_loaded(bio(ontol_db)),
                ensure_loaded(bio(ontol_bridge_from_gosql)),
                class(ID,'cysteine biosynthesis'),
                setof(T-PID,parentT(ID,T,PID),PIDs),
                writeln(parents=PIDs),
                forall(member(_-PID,PIDs),
                       (   class(PID,PN),
                           format(' ~w ~w~n',[PID,PN]))),
                length(PIDs,NumPIDs)),
            (NumPIDs>0))).

unittest(xxtest(recursive2,
            [_=load(go)],
            (   ensure_loaded(bio(ontol_db)),
                ensure_loaded(bio(ontol_bridge_from_gosql)),
                class(ID,'amino acid metabolism'),
                setof(T-PID,parentT(PID,T,ID),PIDs), % down
                writeln(children=PIDs),
                forall(member(_-PID,PIDs),
                       (   class(PID,PN),
                           format(' ~w ~w~n',[PID,PN]))),
                length(PIDs,NumPIDs)),
            (NumPIDs>0))).
/** <module>   
  @author Chris Mungall
  @version  $Revision: 1.8 $
  @date  $Date: 2005/11/23 20:09:16 $
  @license LGPL

  ---+ Name
  ---++ ontol_bridge_from_gosql
- GO MySQL DB adapter

  ---+ Synopsis

  ==
  :- use_module(bio(ontol_db)).
  :- use_module(bio(ontol_bridge_from_gosql)).
  :- ontol_bridge_from_gosql:set_bridge_resource(go).

  % fetch classes from an instance of a GO MySQL DB
  demo:-
    class(ID,'transcription factor activity'),
    format('Class: ~w (TF activity)~n',[ID]),
    forall(synonym(ID,Type,Syn),
           format(' Synonym: [~w] Type: ~w~n',[Syn,Type])),
    forall(subclass(ID,PID),
           (  class(PID,N),
              format(' Superclass: [~w] ~w~n',[PID,N]))).
  ==

  ---+ Description

  This allows an SQL Database using the GO Schema to masquerade as ontol
  predicates

  See <http://www.godatabase.org/dev> GODatabase

  ---++ TODO

  Currently this works by translating calling single SQL queries
whenever a certain ontol_db predicate (eg class/2 is called). For
certain applications, this will result in inefficient code in which
multiple SQL queries are made

  Future versions will employ term rewriting and Draxlers sql_compiler
code to translate entire prolog clauses to multijoin SQL statements
which will be optimised by the DBMS
  
  */