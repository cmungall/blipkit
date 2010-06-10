:- module(ontol_compare,
          [class_set_intersection/3,
           class_set_intersection/4,
	   class_set_difference/4,
	   class_set_difference/5,
	   class_set_difference_with_closest/5
	  ]).
:- use_module(ontol_db).
:- use_module(bio(bioprolog_util),[solutions/3]).

extend_set(Set,SetX,_Opts):-
        solutions(Y,(member(X,Set),parentRT(X,Y)),SetX).

nonredundant(Set,SetNR,_Opts):-
        solutions(X,(member(X,Set),\+((member(Y,Set),parentT(Y,X)))),SetNR).

class_set_intersection(Set1,Set2,Intersection):-
        class_set_intersection(Set1,Set2,Intersection,[]).
class_set_intersection(Set1,Set2,Intersection,Opts):-
        extend_set(Set1,Set1X,Opts),
        extend_set(Set2,Set2X,Opts),
        intersection(Set1X,Set2X,ISet),
        nonredundant(ISet,Intersection,Opts).

class_set_difference(Set1,Set2,Set1Uniq,Set2Uniq):-
	class_set_difference(Set1,Set2,Set1Uniq,Set2Uniq,[]).
class_set_difference(Set1,Set2,Set1UniqNR,Set2UniqNR,Opts):-
        extend_set(Set1,Set1X,Opts),
        extend_set(Set2,Set2X,Opts),
        union(Set1X,Set2X,USet),
	subtract(USet,Set2X,Set1Uniq),
	subtract(USet,Set1X,Set2Uniq),
        nonredundant(Set1Uniq,Set1UniqNR,Opts),
        nonredundant(Set2Uniq,Set2UniqNR,Opts).

class_set_difference_with_closest(Set1,Set2,Set1Matches,Set2Matches,Opts):-
        extend_set(Set1,Set1X,Opts),
        extend_set(Set2,Set2X,Opts),
        union(Set1X,Set2X,USet),
        intersection(Set1X,Set2X,ISet),
	subtract(USet,Set2X,Set1Uniq),
	subtract(USet,Set1X,Set2Uniq),
        nonredundant(Set1Uniq,Set1UniqNR,Opts),
        nonredundant(Set2Uniq,Set2UniqNR,Opts),
	setbestmatches(Set1UniqNR,ISet,Set1Matches),
	setbestmatches(Set2UniqNR,ISet,Set2Matches).

setbestmatches(Set,ISet,SetMatches):-
	solutions(X-Matches,(member(X,Set),bestmatches(X,ISet,Matches)),SetMatches).

bestmatches(C,Set,Matches):-
	solutions(X,bestmatch(C,Set,X),Matches).
bestmatch(C,Set,X):-
	parentT(C,X),
	\+ noparent(X),
	member(X,Set),
	\+ ((parentT(C,Y),
	     parentT(Y,X),
	     member(Y,Set))).





/** <module> perform comparisons of class sets

  ---+ Synopsis

==
:- use_module(bio(ontol_compare)).

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
