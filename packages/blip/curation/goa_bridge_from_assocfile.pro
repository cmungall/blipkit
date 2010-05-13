/* -*- Mode: Prolog -*- */


:- module(goa_bridge_from_assocfile,
          [
          ]).

:- multifile goa_db:goal_expansion/2.

:- use_module(bio(goa_db),[]).
:- use_module(bio(biotable)).

add_bridge_source(Src):-
        (   nb_getval(sources,L)
        ->  true
        ;   L=[]),
        nb_setval(sources,[Src|L]).

bridge_source(Src):-
        nb_getval(sources,L),
        member(Src,L).
setrdb(Rdb):-
        nb_setval(rdb,Rdb).

set_bridge_resource(Resource):-
        rdb_connect(Rdb,Resource),
        setrdb(Rdb).

goa_db:association(DB:FeatID-ClassID,ClassID,DB:FeatID,Qual):-
        bridge_source(Conn),
        setof(in_table(Conn,
                       [
                  proddb(FeatDB),
                  prodacc(FeatLocalID),
                  qualifier(Qual),
                  termacc(ClassID),
                  ref(Ref),
                  evcode(Evid),
                  with(With)
                 ],
                 _Pos).
seqfeature_db:feature(DB:ID,N,Type):-
        bridge_source(Conn),
        in_table(Conn,
                 [
                  proddb(DB),
                  prodacc(ID),
                  prodsymbol(N),
                  prodtype(Type)
                 ],
                 _Pos).
        

% -------------------- TESTS --------------------
% regression tests to ensure behaviour of module is correct;
% lines below here are not required for module functionality

/** <module>   
  @author Chris Mungall
  @version  $Revision: 1.1 $
  @date  $Date: 2005/07/02 00:41:17 $
  @license LGPL

  ---+ Name
%  goa_bridge_from_assocfile

  ---+ Synopsis

  ==
  :- use_module(bio(goa_db)).
  :- use_module(bio(goa_bridge_from_assocfile)).
  :- goa_bridge_from_assocfile:set_bridge_resource(go).

  
  % fetch classes from DB
  demo:-
    class(ID,'transcription factor activity'),
    subclass(ID,PID),
    class(PID,N),
    format('[~w] ~w~n',[PID,N]),
    fail.
  ==

  ---+ Description

  See <http://www.godatabase.org/dev> GODatabase

  */