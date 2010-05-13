:- module(interaction_db,[
                          interacts_with/2,
			  interacts_withS/2
                         ]).

:- use_module(bio(dbmeta)).
:- use_module(bio(graph)).
:- use_module(bio(metadata_db)).

:- extensional(phosphorylates/2).
:- extensional(interacts_with/2).
:- extensional(regulates/2).

interacts_withS(A,B) :- interacts_with(A,B).
interacts_withS(A,B) :- interacts_with(B,A).


/** <module> Model of protein-protein and other interactions

  ---+ Synopsis

==
:- use_module(bio(interaction_db)).

% 
demo:-
  nl.
  

==

---+ Details



---+ Additional Information

This module is part of blip. For more details, see http://www.blipkit.org

@author  Chris Mungall
@version $Revision$
@see     README
@license License


*/
