/* -*- Mode: Prolog -*- */
/**
  @author Chris Mungall
  @version @cvskw $Revision: 1.9 $
  @date @cvskw $Date: 2007/06/18 03:13:40 $
  @license @link(url='http://www.fsf.org/licensing/licenses/lgpl.html')|LGPL|

  @s1|Name| tokenizer - tokenizes atoms to token lists

  @s1 Synopsis

  @cl
  :- use_module(obol(tokenizer)).

  demo:-
    S='abc/def ghi',
    tok_atom(S,X1),
    format('split on space: ~w~n',[X1]),
    tok_atom(S,X2,[ws(" ")]),
    format('split on space: ~w~n',[X2]),
    tok_atom(S,X3,[ws("/")]),
    format('split on slash: ~w~n',[X3]),
    nl.
  
  @/cl

  @s1 Description

**/



:- module(tokenizer,
          [
           tok_atom/2,
           tok_atom/3,
           tok_codes/2,
           tok_codes/3,
           stringify/2
          ]).
           

/**
  @pred tok_atom(+A,?AL)
  @pred tok_codes(+A,+AL,?TL)
  converts an atom into a list of token atoms
  optional tokenlist (as codes); defaults to 9,10,13,32
*/
tok_atom(A,AL):-
        atom_codes(A,CL),
        default_token_list(TL),
        tok_codes(CL,AL,TL).
tok_atom(A,AL,TL):-
        atom_codes(A,CL),
        tok_codes(CL,AL,TL).

/**
  @pred tok_codes(+CL,?AL)
  @pred tok_codes(+CL,?AL,+TL)
  converts a list of tokens into a list of token atoms
  optional tokenlist (as codes); defaults to 9,10,13,32
*/
tok_codes(CL,AL):-
        default_token_list(TL),
        tok_codes(CL,AL,TL).
% (+CL,?AL,+TL)
tok_codes(CL,AL,TL):-
        tok_codes(CL,AL,TL,[]).
% (+CL,?AL,+TL,+BufL) - collects buffer of codes in word-so-far
% BASE CASE
tok_codes([],AL,_,BL):-
        (   BL=[]
        ->  AL=[]
        ;   atom_codes_rev(A,BL),
            AL=[A]).
% WS
tok_codes([C|CL],ALr,TL,BL):-   % ws - add bufword to AL
        whitespace_code(C,TL),
        !,
        (   BL=[]               % ?ws+ (one or more)
        ->  ALr = AL
        ;   atom_codes_rev(A,BL),
            ALr = [A|AL]),
        tok_codes(CL,AL,TL,[]). % clear buffer
% DELIM
tok_codes([C|CL],ALr,TL,BL):-   % delimiter code - preserve delimiter
        is_delim(C,TL),
        !,
        atom_codes(Ca,[C]),
        (   BL=[]               % ?empty buffer
        ->  ALr = [Ca|AL]
        ;   atom_codes_rev(A,BL),
            ALr = [A,Ca|AL]),   % add bufword to AL
        tok_codes(CL,AL,TL,[]). % clear buffer
% PASSTHRU
tok_codes([C|CL],AL,TL,BL):-    % default
        tok_codes(CL,AL,TL,[C|BL]). % add code to bufword


is_word_code(95).
is_word_code(C):-
        C >= 97, C =< 122.
is_word_code(C):-
        C >= 65, C =< 90.
whitespace_code(C,TL):-
        member(ws(WL),TL),
        member(C,WL).
whitespace_code(C,TL):-
        ws_codes(WSCs),
        member(words,TL),
        member(C,WSCs).
is_delim(C,TL):-
        member(words,TL),
        not(is_word_code(C)).
        

ws_codes([9,10,13|" _"]).
%default_token_list([ws(L)]):- ws_codes(L).
default_token_list([words]).

atom_codes_rev(A,CL):-
        reverse(CL,CLr),
        atom_codes(A,CLr).

% stringify(+Toks,?Atom) is semidet
% opposite of tok_atom/2
% fails if Toks contains non-atom
stringify(Ws,S):-
        embed_ws(Ws,Ws2),
        forall(member(W,Ws), % todo - handle more gracefully
               atom(W)),
        concat_atom(Ws2,S).

% transforms token list, adding whitespace characters
% according to certain conventions
% eg ws before opening braces, etc
embed_ws([],[]).
embed_ws([W],[W]):- !.
embed_ws([W,'('|Ws],[W,' ('|Ws2]):-
        !,
        embed_ws(Ws,Ws2).
embed_ws([W,')'|Ws],[W,')'|Ws2]):-
        !,
        (   Ws=[]
        ->  Ws2=[]
        ;   embed_ws([' '|Ws],Ws2)).
embed_ws([W,'.'|Ws],[W,'.',' '|Ws2]):- % same as , TODO: avoid rep
        !,
        (   Ws=[]
        ->  Ws2=[]
        ;   embed_ws(Ws,Ws2)).
embed_ws([W,','|Ws],[W,',',' '|Ws2]):- % same as .
        !,
        (   Ws=[]
        ->  Ws2=[]
        ;   embed_ws(Ws,Ws2)).
/*
embed_ws([W,'.'|Ws],[W,'.'|Ws2]):- % same as , TODO: avoid rep
        !,
        (   Ws=[]
        ->  Ws2=[]
        ;   embed_ws([' '|Ws],Ws2)).
embed_ws([W,','|Ws],[W,','|Ws2]):- % same as .
        !,
        (   Ws=[]
        ->  Ws2=[]
        ;   embed_ws([' '|Ws],Ws2)).
*/
embed_ws([W,'-'|Ws],[W,'-'|Ws2]):-
        !,
        embed_ws(Ws,Ws2).
embed_ws([W,'/'|Ws],[W,'/'|Ws2]):-
        !,
        embed_ws(Ws,Ws2).
embed_ws([W|Ws],[W,' '|Ws2]):-
        !,
        embed_ws(Ws,Ws2).

% -------------------- TESTS --------------------
% regression tests to ensure behaviour of module is correct;
% lines below here are not required for module functionality

test_tokenizer:s_toks('cysteine biosynthesis',[cysteine,biosynthesis]).
test_tokenizer:s_toks('trichome (sensu foo)',[trichome,'(',sensu,foo,')']).
test_tokenizer:s_toks('double-stranded DNA',[double,'-',stranded,'DNA']).
test_tokenizer:s_toks('wibble. foo bar',[wibble,'.',foo,bar]).
%test_tokenizer:s_toks('Serous_sac',['Serous','sac']).

unittest(test(tokenize,
             [],
            (
              forall(test_tokenizer:s_toks(S,Ws1),
                     (   tok_atom(S,Ws),
                         writeln(Ws),
                         Ws=Ws1))),
            true)).
unittest(test(stringify,
             [],
            (
              forall(test_tokenizer:s_toks(S1,Ws),
                     (   stringify(Ws,S),
                         writeln(S),
                         S=S1))),
            true)).
