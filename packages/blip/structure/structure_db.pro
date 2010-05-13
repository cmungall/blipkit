:- module(structure_db,
          [
           pair/2,
           pair/4,
           pos_base/2
           ]).

:- use_module(bio(dbmeta)).

:- extensional(pair/2).

pair(SI,SJ,BI,BJ):-
        pair(SI,SJ),
        pos_base(SI,BI),
        pos_base(SJ,BJ).
:- extensional(pos_base/2).



/** <module> 

  ---+ Synopsis

==
:- use_module(bio(structure_db)).

% 
demo:-
  nl.
  

==

---+ Details



@author  Chris Mungall
@version $Revision$
@see     README
@license License


*/
