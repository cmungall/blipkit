:- module(interaction_db,[
                          interacts_with/2,
			  interacts_withS/2,
                          transitive_interaction/3
                         ]).

:- use_module(bio(dbmeta)).
:- use_module(bio(graph)).
:- use_module(bio(metadata_db)).

:- extensional(phosphorylates/2).
:- extensional(interacts_with/2).
:- extensional(regulates/2).

interacts_withS(A,B) :- interacts_with(A,B).
interacts_withS(A,B) :- interacts_with(B,A).

%% transitive_interaction(+Seeds:list, +MaxDist:int, +Reachable:list) is det
%
% given a set of seed entities (e.g. proteins), find the set of entities
% that can be reached over a path of distance =< MaxDist
transitive_interaction(Seeds,MaxDist,Reachable) :-
        findall(0-Seed,member(Seed,Seeds),Pairs),
        transitive_interaction_impl(Pairs,MaxDist,[],Reachable).

transitive_interaction_impl([],_,Reachable,Reachable) :- !.
transitive_interaction_impl([_-Next|Rest],MaxDist,Accum,Reachable) :-
        memberchk(Next,Accum),
        !,
        transitive_interaction_impl(Rest,MaxDist,Accum,Reachable).
transitive_interaction_impl([Dist-Next|Rest],MaxDist,Accum,Reachable) :-
        Dist >= MaxDist,
        !,
        transitive_interaction_impl(Rest,MaxDist,[Next|Accum],Reachable).
transitive_interaction_impl([Dist-Next|Rest],MaxDist,Accum,Reachable) :-
        setof(With,
              (   interacts_withS(Next,With),
                  \+ member(With,Accum)),
              NewPartners),
        !,
        DistPlus1 is Dist+1,
        findall(DistPlus1-With,member(With,NewPartners),NewPairs),
        append(Rest,NewPairs,List),
        transitive_interaction_impl(List,MaxDist,[Next|Accum],Reachable).
transitive_interaction_impl([_-Next|Rest],MaxDist,Accum,Reachable) :-
        !,
        transitive_interaction_impl(Rest,MaxDist,[Next|Accum],Reachable).


/** <module> Model of protein-protein and other network interactions

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
