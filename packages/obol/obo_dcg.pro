
****

USE obo_dcg!!!!!!



**** 

/*******************************************************

  dcg_term.P - grammar rules for parsing/generating OBO term names

  this module contains a generic grammar suitable for decomposing word
  lists corresponding to OBO terms into parse trees (and vice versa)

  functors:

  np(Stem,Obj)
  pp(Prep,Obj)        eg "in" foo
  rp(Stem,Obj)        eg "cytosolic" membrane
  a(Adj)              eg "inner"
  n(Noun)             eg "transport"
  p(Prep)             eg "of"
  t(Type)             eg cytochrome "c"; "class II"

  expects a database of words and senses

  noun(N,O)
  noun(N,O,right) eg ((x y z w q) pathway)
  noun(N,O,rightmost) eg ((x y z w q) activity)

  right associated nouns can be removed from the rhs of a term
  without affecting the remaining term;
  eg activity, pathway, complex, cascade
  is this always safe???
  "(transforming growth) factor" --?
  
  *******************************************************/

% currently the only words hardcoded into the grammar are
% sensu
% type

:- module(obo_dcg,
          [
           word_type/3,
           dcg_term/3,
           dcg_np/3,
           dcg_rp/3,
           ntnode/3,
           tnode/2,
           tnode/1
          ]).
:- use_module(bio(av_db)).
:- use_module(bio(ontol_db)).

% -------------------
% -- NON-TERMINALS --
% -------------------
% top level; sensu has lowest precedence
dcg_term( NP )        --> dcg_term1(NP).
dcg_term( np(NP,SP )) --> dcg_term1(NP), dcg_sensu(SP).
dcg_term1( np(np(n(N,O)),NP) ) --> dcg_np(NP), [N], {noun(N,rightmost,O)}.
dcg_term1( NP )       --> dcg_np(NP).
dcg_term1( A )        --> dcg_a(A). % special cases; eg "negative"
dcg_term1( T )        --> dcg_type(T). % special cases; eg myosin "II"

                                % treat sensu as preposition
dcg_sensu( pp(p(sensu),NP) ) --> ['(',sensu],dcg_np(NP),[')'].

% :: prepositional phrases: e.g. "(of) (large coated pit) 
dcg_pp( pp(P,NP) )     --> dcg_p(P), dcg_np(NP).
dcg_pp( pp(p(comma),NP) )     --> [','], dcg_np(NP).

% :: conjunctive phrases: e.g. "acyl-CoA (or acyl)"
% initiation of a cp; eg "{or,and} foo"
dcg_cp( cp(C,NP) )     --> dcg_c(C), dcg_np2(0/NP).
% continuation of a cp; eg "x, y and z"; comma takes on meaning of inner cp
dcg_cp( cp(C,np(NP1,cp(C,NP2))) ) --> [','], dcg_np2(0/NP1), dcg_cp( cp(C,NP2) ).

dcg_ca( ca(C,A1,A2) )  --> dcg_a(A1),dcg_c(C),dcg_a(A2). % misfolded or incompletely

% :: relational-adjective phrases; e.g. "vacuolar, segmented, light-activated"
% UHOH-preclude "antigen receptor-mediated" with np4; try np3
% what about "cyclic nucleotide-activated" need np2 not np4? CHANGED TO NP2
% (but only necessary for type pp)
dcg_rp( _/rp(RA) )      --> dcg_rara(RA). % eg cytosolic
dcg_rp( _/rp(RA) )      --> dcg_rapp(RA). % eg segmented
dcg_rp( _/rp(RA,A) )   --> dcg_a(A), ['-'], dcg_rapp(RA). % eg double-stranded
dcg_rp( D/rp(RA,NP) )   --> dcg_np2(D/NP), ['-'], dcg_rapp(RA). % eg light-activated
dcg_rp( D/rp(RA,NP) )   --> dcg_np2(D/NP), dcg_rapp(RA). % eg clathrin coated
%dcg_rp( rp(RA,Adv) )   --> dcg_adv(Adv), dcg_rapp(RA). % eg experimentally modified


% :: noun phrases
% D = depth of search

dcg_np( NP )             --> dcg_np1( 0/NP ).

% precedence=0 NOT YET
%dcg_np0( NP )             --> dcg_np1(NP).
%dcg_np0( np(NP1,NP2) )    --> dcg_np1(NP1), [','], dcg_np1(NP2).

% precedence=1
dcg_np1( D/NP )             --> dcg_np2(D/NP).
%LRdcg_np1( np(NP,PP) )      --> dcg_np1(NP),[','],dcg_pp(PP).       % (x) (of foo)
%LRdcg_np1( np(NP,PP) )      --> dcg_np1(NP),dcg_pp(PP).       % (x) (of foo)

% special right-associative nouns (eg fate;channel) - always modified
%dcg_np1( D/np(np(n(N,Ont)),NP) )    --> dcg_np2(D/NP), [N],
%	{noun(N,right,Ont)}.
%dcg_np1( _/np(n(N,Ont)) )    --> [N],
%	{noun(N,right,Ont)}.

% special syntax for 'symporter activity' terms; lhs and rhs of ':' can
% include adjectives; after the rhs is typically just two nouns 'symp actvty'
% Two terms have x:y:z structure - fail these for now
dcg_np1( D/np(
            np(NP,
               pp(p(lhs),NP1)
              ),
            pp(p(rhs),NP2)
           )
       ) --> dcg_np2(D/NP1),[':'],dcg_np2(0/NP2),dcg_np3(0/NP). % sodium:amino acid symporter activity

% precedence=2; adj and ra
dcg_np2( D/NP        )     --> dcg_np3(D/NP).
dcg_np2( _/np(NP,A)  )     --> dcg_a(A),dcg_np2(0/NP).         % (big) (x)
%%%%%%%%dcg_np2( np(NP,A)  )     --> dcg_a(A),['-'],dcg_np2(NP).         % low-affinity
%LRdcg_np2( np(NP,RP) )     --> dcg_rp(RP),dcg_np2(NP).       % (foo-ed) (x)
%%%%%%%%dcg_np2( np(NP2,NP1) )   --> dcg_np4(NP1), ['-'], dcg_np4(NP2). % eg volume-sensitive [ADD NEW GRAMMAR CLASS?]
% type phrases (eg class II)
%LRdcg_np2( np(NP,TP) )      --> dcg_np2(NP),dcg_tpr(TP).        % (x) (- II) !LR
dcg_np2( _/np(NP,TP) )      --> dcg_tpl(TP), dcg_np2(0/NP).       % class II (x)
% negation
dcg_np2( _/c(non,NP) )     --> [non],['-'],dcg_np2(0/NP). % non-mammalia

% precedence=3
dcg_np3( D/NP )             --> dcg_np4(D/NP).
dcg_np3( D/np(NP2,NP1) )    --> {inc_depth(D,D2)},dcg_np2(D2/NP1), dcg_np2(0/NP2).    % np modifiers !LR
%LRdcg_np3( np(NP,CP) )      --> dcg_np1(NP),dcg_cp(CP).       % (x) (and foo) !LR

% precedence=4
dcg_np4( _/np(N) )         --> dcg_n(N),{writeln(n=N)}.                    % single-noun

% :: type phrases: e.g. "class II -"
dcg_tpr( tp(qualifier,T)      )   --> dcg_type(T). % myosin (II)
dcg_tpr( tp(has_class,T)      )   --> [class],dcg_type(T). % MHC (class II)
dcg_tpr( tp(has_type,T)       )   --> [type],dcg_type(T). % collagen (type II)
dcg_tpr( tp(has_typeh,T)      )   --> ['-'],dcg_type(T). % acyl(-CoA)??

dcg_tpl( tp(has_prefix_type,T))   --> [type],dcg_type(T). % (type II) diabetes
dcg_tpl( tp(has_prefixh,T)    )   --> dcg_type_prefix(T),['-']. % (T-)cell
dcg_tpl( tp(has_prefix,T)     )   --> dcg_type_prefix(T). % (B) cell

% ---------------
% -- TERMINALS --
% ---------------
dcg_rara( ra(A,N,ra,O)   ) --> [A], {relational_adj(A,N,ra,O)}.  % eg cytosolic
dcg_rapp( ra(A,N,pp,O)   ) --> [A], {relational_adj(A,N,pp,O)}.  % eg activated

dcg_ra( ra(A,N,T,O)   ) --> [A], {relational_adj(A,N,T,O)}.     % any rel-adj
dcg_c( c(C,O) )      --> [C], {conj(C,O)}.                       % and/or
%dcg_c( c(not(C),O) ) --> [C],[not] {conj(C,O)}.                  % and/or not
dcg_a( a(A,O) )      --> [A], {adj(A,O)}.                       % eg inner
dcg_p( p(P)   )      --> [P], {prep(P)}.                        % eg in, within
dcg_n( n(N,O) )      --> [N], {allowed_noun(N,O)}.                      %
%dcg_n( n(N,O) )      --> [X],[Y], {atomic_pair(X,Y,N,O)}.
dcg_n( n(N,orphan) ) --> [N], {not(known_word(N))}.

dcg_n( n(N,O) )      --> [N], {class(ID,N),belongs(ID,O)}.

% WEIRD SPECIAL CASE STUFF...
dcg_type( t(T,O)          ) --> [T], {ttok(T,O)}.                 % eg mysoin II
dcg_type_prefix( t(T,O)   ) --> [T], {ttok(T,prefix,O)}.          % eg B cell

% -- orphan noun tokenlists (eg down a concentration gradient)
% we glom these together to prevent combinatorial explosions from NP->NP NP rule
% [we can obsolete this rule once we have a better solution for 'chatty' terms]
dcg_n( n(tlist(N),orphan) ) --> dcg_zn(N).

dcg_znt( N )         --> [N], {not(known_word(N))}.
dcg_zn( [N1,N2]    )     --> dcg_znt(N1),dcg_znt(N2).
dcg_zn( [N|NL] )  --> dcg_znt(N), dcg_zn(NL).

% do we lose 'transcription' in transcription factor?
%dcg_rn1( RN     ) -->  dcg_rn2(RN).
%dcg_rn1( n([N|NL],O1) ) -->  [N], dcg_rn2(n(NL,O1)), {noun(N,O)}.

%dcg_rn2( [N] )    -->  [N], {noun(N,right,O)}.
%dcg_rn2( [N|NL] ) -->  [N], dcg_rn2(NL), {noun(N,right,O)}.

inc_depth(D,D2):-
        D =< 1,
        D2 is D+1.

% -- chemical compounds are awkward to deal with - the tokenizer
% splits on some of the symbols below, but we don't want to treat
% each part of a compound name as seperate words
compound_symbol('-').
compound_symbol('(').
compound_symbol(')').
compound_symbol('[').
compound_symbol(']').
compound_symbol(',').
compound_symbol(':').

% -- rules --
relational_adj(A,N,pp,O):-  
	relational_adj_pp(A,N,O).
relational_adj(A,N,ra,O):-  
	relational_adj_ra(A,N,O).
ttok(T,TP,O):-
	type_token(T,TP,O).
ttok(T,O):-
	type_token(T,O)
        ;
	roman_numeral(T,_,O)
        ;
	greek(T,O)
        ;
        number(T,O).

prep(X):- prep(X,_).
adj(X):- adj(X,_).   % redundant???


% some nouns are not part of the vocabulary, but they are
% implicitly there via relational_adjs
% ANYNOUN - any at all
anynoun(N,O):- noun(N,_,O).     % right/rightmost-assoc noun
anynoun(N,O):- allowed_noun(N,O).
% right and rightmost nouns are not allowed in general contexts

% ===============================================================
% ntnode(?Type,?ChildList,+Node) det PUBLIC
% ===============================================================
ntnode(np,[X],np(X)).
ntnode(np,[X,Y],np(X,Y)).
ntnode(pp,[Y,X],pp(X,Y)).
ntnode(cp,[Y,X],cp(X,Y)).
ntnode(rp,[X],rp(X)).
ntnode(rp,[X,Y],rp(X,Y)).
ntnode(tp,[Y],tp(_,Y)).

% ===============================================================
% tnode(+Node) det PUBLIC
% ===============================================================
tnode(X):-tnode(X,_).
% ===============================================================
% tnode(+Node,?Type) det PUBLIC
% ===============================================================
tnode(n(N,O),N).
tnode(v(N,O),N).
tnode(p(N),N).
tnode(a(N,O),N).
tnode(c(N,O),N).
tnode(t(N,O),N).
tnode(ra(N,_,_,_),N).

det(a).
det(an).
det(the).

% -------------------- TESTS --------------------
% regression tests to ensure behaviour of module is correct;
% lines below here are not required for module functionality

unittest(load(av)=
      load_bioresource(obol_av)/[]).
unittest(load(go)=
      load_bioresource(go)/[]).

unittest:test_term('binding site').
unittest(test(test1,
             [_=load(av),
              _=load(go)],
            (
              %leash(-all),trace,
              ensure_loaded(bio(ontol_db)),
              ensure_loaded(bio(tokenizer)),
              forall(test_term(T),
                     unittest:t_parse(T))),
            true)).

unittest:t_parse(S):-
        format('parsing ~w~n',[S]),
        tokenizer:tok_atom(S,WL),
        format('tokens ~w~n',[WL]),
        setof(T,dcg_term(T,WL,[]),Ts),
        forall(member(T,Ts),
               writeln(t=T)).

