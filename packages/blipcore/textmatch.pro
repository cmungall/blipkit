/* -*- Mode: Prolog -*- */



:- module(textmatch,
          [textmatch/2,
           textmatch/3]).
:- use_module(bio(mode)).

:- mode textmatch(+,+) is nondet.
textmatch(Text,Search):-
        textmatch(Text,Search,_).

%% textmatch(+Text,+SearchTerm,?Position)
%  SearchTerm =
%   exact(Text) |
%   contains(Match) |
%   begins(Match) |
%   token_search(Match) |
%   search(SearchAtom)
%
%  search(SearchAtom) takes an SQL-style search string, with the %
%  character matching anything
%
%  SearchTerm can also be a two-argument term, with the second argument
%  indicating whether to perform porter_stemming (0=no,1=yes)
%  
%  
:- mode textmatch(+,+,?) is nondet.
textmatch(Text,exact(Text,0),pos(0)).
textmatch(Text,exact(Match,1),pos(0)):-
        atom_to_stem_list(Match,L),
        atom_to_stem_list(Text,L).
textmatch(Text,exact(Text),pos(0)).

textmatch(Text,search(Atom,0),pos(P)):-
        textmatch(Text,search(Atom),pos(P)).
textmatch(Text,search(Atom,1),pos(P)):-
        concat_atom(Parts,'%',Atom),
        maplist(stem_atom,Parts,PartsStemmed),
        atom_to_stem_list(Text,TextTokens),
        concat_atom(TextTokens,TextStemmed),
        regmatch(TextStemmed,PartsStemmed,0,P).        
textmatch(Text,search(Atom),pos(P)):-
        concat_atom(Parts,'%',Atom),
        regmatch(Text,Parts,0,P).        

textmatch(Text,contains(Match),pos(Pos)):-
        sub_atom(Text,Pos,_,_,Match).
textmatch(Text,contains(Match,0),pos(Pos)):-
        textmatch(Text,contains(Match),pos(Pos)).
textmatch(Text,contains(Match,1),pos(Pos)):-
        stem_atom(Text,TextStemmed),
        stem_atom(Match,MatchStemmed),
        sub_atom(TextStemmed,Pos,_,_,MatchStemmed).

textmatch(Text,begins(Match),pos(0)):-
        sub_atom(Text,0,_,_,Match).

textmatch(Text,token_search(Match),pos(0)):-
	ensure_loaded(library(porter_stem)), % we tokenize_atom/2
        tokenize_atom(Text,InputTokens),
        tokenize_atom(Match,MatchTokens),
        sublist(MatchTokens,InputTokens).

stem_atom(Atom,AtomStemmed):-
        atom_to_stem_list(Atom,Stems),
        concat_atom(Stems,AtomStemmed).

% regmatch(+Text,+Parts,?ForceStartPosition,?Start)
:- mode regmatch(+,+,+,?) is nondet.
:- mode regmatch(+,+,-,?) is nondet.
regmatch('',[],_,_):-
        !.                      % entire Text must be consumed
regmatch(_,[''],_,_):-          % trailing '' means the search pattern
        !.                      % ended with a '%' wildcard
regmatch(Text,[''|Parts],_,P):- % wildcard 
        !,
        regmatch(Text,Parts,_,P). % call with ForceStartPosition nonvar
regmatch(Text,[Part|Parts],PStart,PStart):-
        sub_atom(Text,PStart,Len,_,Part),
        PNext is PStart+Len,
        sub_atom(Text,PNext,_,0,TextRest), % consume
        regmatch(TextRest,Parts,_,_). % ignore succeeding PStarts

% -------------------- TESTS --------------------
% regression tests to ensure behaviour of module is correct;
% lines below here are not required for module functionality

unittest(test(match,
            [],
            (   findall(P,textmatch(abcfnorddeffnordxyzfnord,
                                    contains(fnord),
                                    P),
                        Ps),
                writeln(ps=Ps),
                nl
                ),
            member(pos(19),Ps))).
                 

unittest(test(regmatch,
            [],
            (   T=abcfnorddeffnordxyzfnord,
                findall(P,textmatch(T,
                                    search('%fnord%'),
                                    P),
                        Ps),
                findall(P,textmatch(T,
                                    search('%fnord'),
                                    P),
                        Ps2),                
                writeln(ps=Ps),
                writeln(ps2=Ps2),
                nl
                ),
            (   Ps=[pos(3),pos(11),pos(19)],
                Ps2=[pos(19)]))).
                 

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
  :- use_module(bio(textmatch)).

  ==

  ---+ Description

**/