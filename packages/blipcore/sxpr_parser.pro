/* -*- Mode: Prolog -*- */


:- module(sxpr_parser,
          [
           file_sxprs/2,
           stream_sxprs/2,
           codes_sxprs/2,
           prolog_sxpr/2,
           sxpr_prolog/2
          ]).
:- use_module(bio(bioprolog_util)).
:- [parser_general].

io:parse_stream(sxpr,IO):-
        stream_sxprs(IO,Xs),
        maplist(sxpr_prolog,Xs,Facts),
        maplist(assert,Facts).

file_sxprs(F,Xs):-
        debug(sxpr,'Reading file to codes',[]),
        read_file_to_codes(F,Codes,[]),
        codes_sxprs(Codes,Xs).

stream_sxprs(IO,Xs):-
        debug(sxpr,'Reading stream: ~w',[IO]),
        read_stream_to_codes(IO,Codes),
        debug(sxpr,'Reading codes',[]),
        codes_sxprs(Codes,Xs).

codes_sxprs(Codes,Xs):-
        toks(Toks1,Codes,[]),
        debug(sxpr,'Removing comments',[]),
        %strip_comments(Toks1,Toks),
	Toks=Toks1,
        debug(sxpr,'Parsing',[]),
        slists(Xs,Toks,[]).

sxpr_prolog([],[]):- !.
sxpr_prolog([H|Args],Term):-
        !,
        maplist(sxpr_prolog,Args,Args2),
        Term =.. [H|Args2].
sxpr_prolog(X,X).

prolog_sxpr(X,X):- var(X),!.
prolog_sxpr(X,X):- atomic(X),!.
prolog_sxpr(X,L2):-
        X=..L1,
        maplist(prolog_sxpr,L1,L2).


strip_comments([],[]):- !.
strip_comments([comment(_)|In],Out):- !,strip_comments(In,Out).
strip_comments([H|In],[H|Out]):- !,strip_comments(In,Out).


open_xpr --> ['('].
close_xpr --> [')'],!.
close_xpr --> {writeln('expected ")"'),fail}.

slists([H|L]) --> slist(H),!,slists(L).
slists([]) --> [],!.
slist(L) --> open_xpr,!,slist_rest(L).
slist($comment(X)) --> [comment(X)].

slist_rest(L) --> terms(L),close_xpr,!.
slist_rest(_) --> terms(L),{writeln('failed: '-L),fail}.

terms([H|L]) --> term(H),!,terms(L).
terms([]) --> [],!.
term(X) --> slist(X),!.
term(X) --> [X],{X\=')'}.


tok('(') --> "(".
tok(')') --> ")".
tok(X) --> """",token(X^""""),!,"""".
tok(comment(X)) --> ";",token(X^"\n"),!.
tok(X) --> token(X^"(); \n\r").


toks([H|L]) --> ws_star,tok(H),!,toks(L).
toks([]) --> ws_star.


% -------------------- TESTS --------------------
% regression tests to ensure behaviour of module is correct;
% lines below here are not required for module functionality


unittest(test(sxpr1,
            [],
            (   ensure_loaded(bio(sxpr_parser)),
                file_sxpr('test.kif',X),
                writeln(X),
                nl
                ),
            true)).

/** <module>   
  @author Chris Mungall
  @version  $Revision: 1.6 $
  @date  $Date: 2005/08/24 23:51:06 $
  @license LGPL

  ---+ Name
  ---++ sxpr_parser


  ---+ Synopsis
  
  ==
  ==

  ---+ Description

  
  ---++ See also
  

  */
