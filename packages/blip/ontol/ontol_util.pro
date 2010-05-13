/* -*- Mode: Prolog -*- */


:- module(ontol_util,
          [
           matching_term_pair/1,
           matching_term_pair/2,
           matching_term_pair/3
           ]).
:- use_module(bio(ontol_db)).

matching_term_pair(N):-
        matching_term_pair(_,_,N).
matching_term_pair(ID1,ID2):-
        matching_term_pair(ID1,ID2,_).
matching_term_pair(ID1,ID2,N):-
        belongs(ID1,Ont1),
        belongs(ID2,Ont2),
        Ont1\=Ont2,
        class(ID1,N),
        class(ID2,N).
/** <module>   
  @author Chris Mungall
  @version  $Revision: 1.4 $
  @date  $Date: 2005/06/22 02:10:33 $
  @license LGPL


  ==
  :- use_module(bio(ontol_util)).
  ==


  */