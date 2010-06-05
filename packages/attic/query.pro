/* -*- Mode: Prolog -*- */



:- module(query,
          []).
:- use_module(library(porter_stem,[tokenize_atom/2])).

textmatch(Text,Search):-
        textmatch(Text,Search,_).
textmatch(Text,exact(Text),pos(0)).
textmatch(Text,contains(Match),pos(Pos)):-
        sub_atom(Text,Pos,_,_,Match).
textmatch(Text,begins(Match),pos(0)):-
        sub_atom(Text,0,_,_,Match).
textmatch(Text,token_search(Match),pos(0)):-
        tokenize_atom(Text,InputTokens),
        tokenize_atom(Match,MatchTokens),
        sublist(MatchTokens,InputTokens).

        
/** <module>
  @author Chris Mungall
  @version  $Revision$
  @date  $Date$
  @license LGPL

  ---+ Name
  ---++ query
- 

  ---+ Synopsis

  ==
  :- use_module(bio(query)).

  ==

  ---+ Description

**/