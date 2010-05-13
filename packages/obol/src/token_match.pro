/* -*- Mode: Prolog -*- */
/**
  @author Chris Mungall
  @version @cvskw $Revision: 1.4 $
  @date @cvskw $Date: 2006/07/25 01:55:38 $
  @license @link(url='http://www.fsf.org/licensing/licenses/lgpl.html')|LGPL|

  @s1|Name| token_match - 

  @s1 Synopsis

  @cl
  :- use_module(token_match).
  :- use_module(library(porter_stem)).

  desc('FM1-43 assay demonstrates the mutation impairs the formation of endocytotic synaptic vesicles').
  
  demo:-
    forall(desc(Desc),
           (   atom_to_stem_list(Desc,Toks),
               tokens_to_class_matches(Toks,Matches,[stem(1)]),
               writeln(Matches))).
  
  @/cl

  @s1 Description

**/

:- module(token_match,
          [tokens_to_class_matches/3,
           class_label_stemmed/3]).
:- use_module(library(porter_stem)).
:- use_module(bio(ontol_db)).
:- use_module(bio(mode)).

class_label_stemmed(ID,Label,SynType):-
        class_label_stemmed(ID,Label,SynType,[]).
class_label_stemmed(ID,Label,SynType,Opts):-
        class_label(ID,Label1,SynType),
        atom_to_stem_list(Label1,Stems),
        concat_atom(Stems,' ',Label),
        \+ (   member(min_label_length(MinLen),Opts),
               atom_length(Label,LabelLen),
               LabelLen < MinLen).
                      

/**
  @pred tokens_to_class_matches(+Tokens,?Matches,+Opts)
  Options can be stem(ZeroOrOne), exclude(ClassID)

  Matches=[Token-[match(ID,Label,Name,SynType),...],....]
  
  true if Matches can be found in Tokens
  */
:- mode tokens_to_class_matches(+,?,+) is det.
tokens_to_class_matches(TL,ML,Opts):-
        (   member(stem(1),Opts)
        ->  maplist(porter_stem,TL,TL1)
        ;   TL1=TL),
        tokens_to_class_matches1(TL1,ML,Opts).

:- mode tokens_to_class_matches1(+,?,+) is det.
% we could just do sub_atom/5, but we try and improve on this by seeding
% match set by only checking from first character
%   (why not just seed then do normal sub_atom/5...?
%    coz we want to find all, longest, no overlaps.)
% seed match with first input token - find all classes that start
% with the same token.
tokens_to_class_matches1([T|TL],[T2-ML2|MTL],Opts):- % two or more left
        findall(match(ID,Label,N,SynType),
                (   (   member(stem(1),Opts)
                    ->  class_label_stemmed(ID,Label,SynType,Opts)
                    ;  class_label(ID,Label,SynType)),
                    \+ member(exclude(ID),Opts),
                    sub_atom(Label,0,_,_,T),
                    class(ID,N)),
                SL),
        findall(M,              % exact
                (member(M,SL),M=match(ID,T,_,_)),
                ML),
        extend_match(T,TL,SL,ML,rest(T2,ML2,TL2)),
        tokens_to_class_matches1(TL2,MTL,Opts).
tokens_to_class_matches1([],[],_).          % no more; returned Matches empty

:- mode extend_match(+,+,+,+,?) is det.
extend_match(T,TL,SL,ML,R):-
        extend_match1(T,TL,SL,ML,R),
        !.
% failed to extend: empty or no more matches
% current is best
extend_match(T,TL,_,ML,rest(T,ML,TL)). 
        
:- mode extend_match1(+,+,+,+,?) is det.
extend_match1(T0,[T1|TL],SL,_,R):-
        concat_atom([T0,T1],' ',T),
        findall(S,
                (   member(S,SL),
                    S=match(ID,N,_,_),
                    sub_atom(N,0,_,_,T)),
                SL1),
        SL1\=[],                % fail unless can extend
        !,
        findall(M,              % exact
                (member(M,SL1),M=match(ID,T,_,_)),
                ML),
        extend_match(T,TL,SL1,ML,R).
