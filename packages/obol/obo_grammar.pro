
/* -*- Mode: Prolog -*- */
/**
  @author Chris Mungall
  @version @cvskw $Revision: 1.15 $
  @date @cvskw $Date: 2007/06/09 21:45:23 $
  @license @link(url='http://www.fsf.org/licensing/licenses/lgpl.html')|LGPL|

  @s1|Name| obo_grammar - syntactic parses of obo language sentences

  @s1 Synopsis

  @cl
:- use_module(name).

  @/cl

  @s1 Description

  this module contains a generic grammar suitable for decomposing word
  lists corresponding to OBO terms into parse trees (and vice versa)

  The grammar uses left-recursive rules, so a chart parsing strategy
is used (Earley's algorithm). See @rp|earley_parse/4|

*/

% currently the only words hardcoded into the grammar are
% sensu
% type

:- module(obo_grammar,
          [
           term_processing_opts/1,
           parse_term/2,
           parse_tokens/2,
           tree_tokens/3,
           term_tree/1,
           tnode/1,
           tree_to_tokens/2
          ]).
:- use_module(earley).
:- use_module(tokenizer).
:- use_module(bio(av_db)).
:- use_module(bio(ontol_db)).
:- use_module(library(porter_stem)).

:- dynamic term_processing_opts_dyn/1.

:- op(1200,xfx,'--->').

/**
  @pred term_processing_opts(Opts)
  @desc if Opts is var, unifies with current settings or [] if not set
  if Opts is nonvar, sets current settings

  Possible Opts are
  @list
  @li stem(1)
  perform stemming on all term tokens
  @/list
  */
term_processing_opts(Opts):-
        (   var(Opts)
        ->  (   term_processing_opts_dyn(Opts)
            ->  true
            ;   Opts=[])
        ;   retractall(term_processing_opts_dyn(_)),
            assert(term_processing_opts_dyn(Opts))).

/**
  @pred parse_term/2
  @mode parse_term(+S,?Tree)
  @mode parse_term(?S,+Tree)
  @arg|S| a sentence atom
  @arg|Tree| a parse tree
  */
parse_term(S,Tree):-
        term_processing_opts(Opts),
        (   member(stem(1),Opts)
        ->  atom_to_stem_list(S,Ws)
        ;   tok_atom(S,Ws)),
                                %maplist(process_token(Opts),Ws,Ws2),
        tree_tokens(Tree,Ws,[]).

/**
  @pred parse_tokens/2
  @mode parse_tokens(+Ws,?Tree)
  @mode parse_tokens(?Ws,+Tree)
  @arg|Ws| a sentence as a list of atom tokens
  @arg|Tree| a parse tree
  */
parse_tokens(S,T):- tree_tokens(T,S,[]).

process_token([],Tok,Tok).
process_token([Opt|Opts],Tok,Tok2):-
        process_token1(Opt,Tok,Tok1),
        process_token(Opts,Tok1,Tok2).
process_token1(stem(1),Tok,Tok2):-
        !,
        porter_stem(Tok,Tok2).
process_token1(_,Tok,Tok).

/**
  @pred term_tree(+Tree)
  @arg|Tree| a parse tree

  semidet - succeeds if Tree is a valid tree
  */
term_tree(T):-
        writeln(T),
        earley:chart(T,_,[],[]).

tree_tokens(T,S,R):- earley_parse(obo_grammar,T,S,R),T=term(_).

% -------------------
% -- NON-TERMINALS --
% -------------------

term( NP )        ---> term1(NP).
term( np(NP,SP )) ---> term1(NP), sensu(SP).
term1( NP )       ---> np(NP).
term1( A )        ---> a(A).    % special cases; eg "negative"
term1( A )        ---> ap(A).   % special cases; eg "dorsal/ventral"
term1( T )        ---> otype(T). % special cases; eg myosin "II"
term1( T )        ---> rp(T).   % special cases; eg light-activated

% :: sensu ::
sensu( pp(prep(sensu),NP) ) --->
        t_atom('('),t_reserved(sensu),np(NP),t_atom(')').
sensu( pp(prep(sensu),NP) ) --->
        t_reserved(sensu),np(NP).
sensu( pp(prep(sensu),NP) ) --->
        t_atom('('),t_reserved(sensu),np(NP).

% :: prepositional phrases: e.g. "(of) (large coated pit)"
pp( pp(P,NP) )               ---> p(P), np(NP).
pp( pp(prep(comma),NP) )     ---> t_atom(','), np(NP).

% :: relational adjective phrases:
rp( rp(RA) )      ---> ra(RA). % eg cytosolic
rp( rp(RA,A) )    ---> a(A),t_atom('-'),ra(RA,pp). % eg double-stranded
rp( rp(RA,NP) )   ---> np2(NP),t_atom('-'),ra(RA,pp). % eg light-activated
rp( rp(RA,NP) )   ---> np2(NP),ra(RA,pp). % eg light activated
rp( rp(RA,NP) )   ---> np2(NP),ra(RA,pp). % eg clathrin coated

% :: opaque types: class II, type A, alpha, etc
otype( t(T) )     ---> t_symbol(T). % alpha
otype( t(T) )     ---> t_atom(class), t_symbol(T). 
otype( t(T) )     ---> t_atom(type), t_symbol(T). 
otype( t(T) )     ---> t_number(T).
%otype( ct(T,N) )  ---> t_symbol(T),t_atom('-'),t_number(N).

% :: unknown-phrase - treat as NP. non-LR parse
up( np(U) )         ---> u(U).
up( np(U,U2) )      ---> u(U),up(U2).

% noun-phrase have different levels of precedence
np( NP ) ---> t_atom(the),np1(NP).     % det
np( NP ) ---> np1(NP).          % prepositional phrases
np1( NP ) ---> np2(NP).         % adjs, relational adjs, type tokens
np2( NP ) ---> np3(NP).         % np-np
np3( NP ) ---> np4(NP).         % nouns, unknown-phrases

np1( np(NP,PP) )   ---> np1(NP),pp(PP).       % (x) (of foo)
np1( np(NP,PP) )   ---> np1(NP),t_atom(','),pp(PP). % (x) (of foo)
np1( np(NP,RP) )   ---> np1(NP),t_atom(','),rp(RP). % foo, soma encoded
np2( np(NP,A) )    ---> a(A),np2(NP). % large subunit 
np2( np(NP,A) )    ---> np2(NP),a(A). % germ cell morphology abnormal [this syntax used in worm_phenotype]
np2( np(NP,AP) )   ---> ap(AP),np2(NP). % dorsal/ventral axis
np2( bopp(Op,conj(np(NP,A1),np(NP,A2))) ) --->
        a(A1),bop(Op),a(A2),np(NP). % generative and vegetative cell
np2( np(NP,RP) )   ---> rp(RP),np2(NP).       % (foo-ed) (x)
np2( np(NP,T) )    ---> otype(T),t_atom('-'),np(NP).
np2( np(NP,T) )    ---> otype(T),np(NP).
np2( np(NP,T) )    ---> np(NP),otype(T).
np3( np(NP2,NP1) ) ---> np2(NP1),np2(NP2).
np3( bopp(Op,conj(NP1,NP2)) ) --->
        np2(NP1),bop(Op),np2(NP2).
np4( UP    )       ---> up(UP). % eg xyz wibble foo
np4( np(N)    )    ---> n(N).


% eg dorsal/ventral
ap( ap(S,conj(A1,A2)) ) ---> a(A1),t_separator(S),a(A2).


% ---------------
% PRE-TERMINALS
% ---------------
% (these seem to be necessary in the Earley framework to
%  be able to type the terminals)
p( prep(P) )         ---> t_p(P).
a( adj(A) )          ---> t_a(A).
n( noun(N) )         ---> t_n(N).
u( unk(W)  )         ---> t_u(W).
bop( bop(W) )        ---> t_bop(W).
bop( bop('and/or') ) ---> t_bop(and),t_separator('/'),t_bop(or).
ra( A )              ---> ra(A,_).
ra( reladj(A),pp )   ---> t_ra_pp(A).
ra( reladj(A),ra )   ---> t_ra_ra(A).


% ---------------
% -- TERMINALS --
% ---------------
word(t_p,W):- prep(W,_).
word(t_a,W):- adj(W,_).
word(t_n,W):- noun(W,_).
word(t_det,W):- det(W).
word(t_symbol,W):- ttok(W,_).
word(t_n,W):- class_exact_name(ID,W),\+ word_type(W,_), \+ lexical_category(ID,adj).
word(t_a,W):- class_exact_name(ID,W),lexical_category(ID,adj),\+ word_type(W,_).
word(t_ra_pp,W):- relational_adj(W,_,pp,_).
word(t_ra_ra,W):- relational_adj(W,_,ra,_).
word(t_reserved,W):- reserved_word(W).
word(t_u,W):- \+ known_word(W), \+ class_exact_name(_,W).
word(t_bop,W):- binary_operator(W).
word(t_number,W):- is_atom_number(W).
word(t_atom,',').
word(t_atom,'-').
word(t_atom,'(').
word(t_atom,')').
word(t_separator,'/').            % dorsal/ventral
word(t_separator,':').            % 

is_atom_number(A):-  atom_codes(A,CL),name(A2,CL),number(A2).

% TODO
% -- chemical compounds are awkward to deal with - the tokenizer
% splits on some of the symbols below, but we don't want to treat
% each part of a compound name as seperate words
compound_symbol('-').
compound_symbol('(').
compound_symbol(')').
compound_symbol('[').
compound_symbol(']').
%compound_symbol(',').
compound_symbol(':').


tnode(W):- word(_,W).

det(a).
det(an).
det(the).

binary_operator(and).
binary_operator(or).

class_exact_name(Class,Name):- class(Class,Name).
class_exact_name(Class,Name):- synonym(Class,exact,Name).
class_exact_name(Class,Name):- synonym(Class,stemmed,Name).
class_exact_name(Class,Name):- synonym(Class,'EXACT',Name).
class_exact_name(Class,Name):- synonym(Class,'STEMMED',Name).

tree_to_tokens(T,Ws):-
        apply_rule(T,Ws,[]).

pairs_to_list((H,T),[H|T1]):-
        !,
        pairs_to_list(T,T1).
pairs_to_list(H,[H]).

rule(H,B,true):- rule(H,B).
rule(H,Bs):-
        (H ---> B),
        pairs_to_list(B,Bs).

apply_rules([],T,T).
apply_rules([R|Rs],Ws,Tail2):-
        apply_rule(R,Ws,Tail),
        apply_rules(Rs,Tail,Tail2).

apply_rule(R,[W|T],T):-
        R=..[F,W],
        word(F,W).
apply_rule(H,Ws,Tail):-
        rule(H,Bs),
        apply_rules(Bs,Ws,Tail).
