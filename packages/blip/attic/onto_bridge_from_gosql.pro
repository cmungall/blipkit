/* -*- Mode: Prolog -*- */


:- module(ontol_bridge_from_gosql,
          [
          ]).

:- use_module(bio(ontol_db)).
:- use_module(bio(rdb_util)).

set_bridge_resource(Resource):-
        rdb_connect(Rdb,Resource),
        nb_setval(rdb,Rdb).

ontol_db:class(ID,N):-
        nb_getval(rdb,Rdb),
        sql_lookup(Rdb,term,[acc=ID,name=N]).

/** <module>   
  @author Chris Mungall
  @version  $Revision: 1.1 $
  @date  $Date: 2005/06/22 02:16:15 $
  @license LGPL


  ==
  :- use_module(bio(ontol_bridge_from_gosql)).
  :- ontol_bridge_from_gosql:set_bridge_resource(go).

  demo:-
    class(ID,N),
    format('[~w] ~w~n',[ID,N]),
    fail.
  ==

  */