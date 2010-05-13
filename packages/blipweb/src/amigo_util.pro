/* -*- Mode: Prolog -*- */
/** @copyright
  
  Copyright (C) 2005 by Chris Mungall (cjm AT fruitfly DOT org)
  
  @/copyright

  General utility code for amigo
  
  */

:- module(amigo_util,
          [
           splicelist/5,
           list_bin/4,
           xsetof/3
          ]).

%%%%%%%%%%%%%%%%%%%%
% -- Utils --
%%%%%%%%%%%%%%%%%%%%

/**
  @pred splicelist(+List,+From,+Len,?SplicedList,?ListRemaining) det
   splices elements within range from a list
*/
% (+,+,+,?,?) sd
splicelist([],_,_,[],[]):- !.
splicelist(L,0,0,[],L):- !.
splicelist([H|L],0,Len,[H|Ls],Rem):-
        !,
        Len1 is Len-1,
        splicelist(L,0,Len1,Ls,Rem).
splicelist([H|L],From,Len,Ls,[H|Rem]):-
        !,
        From1 is From-1,
        splicelist(L,From1,Len,Ls,Rem).

% (+,+,?,?) nd
% splits a list of N elements into scroll bins of size W, from B..E
% base1 arith
list_bin(N,W,B,E):-
        W>0,
        NW is truncate((N-1)/W),
        numlist(0,NW,IL),
        member(I,IL),
        B is ((I*W) + 1),
        E1 is ((I+1)*W),
        (E1 > N
        -> E=N
        ;  E=E1).

xsetof(X,F,L):-
        (setof(X,F,L) -> true ; L=[]).


