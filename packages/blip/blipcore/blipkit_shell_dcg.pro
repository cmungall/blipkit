/* -*- Mode: Prolog -*- */



:- module(blipkit_shell_dcg,
          [tokenize_blip_command/2]).

%% tokenize_blip_command(+S,?Args) is semidet.
tokenize_blip_command(S,Args):-
        atom_codes(S,Codes),
        words(Args,Codes,[]).

% tokenize blip shell args
words([W|L]) --> word(W),!,words(L).
words([]) --> [].
word(W) --> toks(L),!,{L\=[],atom_codes(W,L)}.
toks([]) --> ws_plus,!.
toks(L) --> "\"",!,toks_qt(L).
toks([X|L]) --> "\\",[X],!,toks(L).
toks([X|L]) --> [X],!,toks(L).
toks([]) --> [].
toks_qt([]) --> "\"",!,ws_star.
toks_qt([X|L]) --> [X],!,toks_qt(L).

% greedy
ws_plus --> " ",!,ws_star.
ws_star --> " ",!,ws_star.
ws_star --> [].

blip_command([BL|L],[],0) --> " ",!,blip_command(L,BL,0).
blip_command(L,[X|BL],0) --> "\"",!,[X],blip_command(L,BL,1).
blip_command(L,[X|BL]) --> [X],!,blip_command(L,BL).
/** <module>
  @author Chris Mungall
  @version  $Revision$
  @date  $Date$
  @license LGPL

  ---+ Name
  ---++ blipkit_shell_dcg
- 

  ---+ Synopsis

  ==
  :- use_module(bio(blipkit_shell_dcg)).

  ==

  ---+ Description

**/