/* -*- Mode: Prolog -*- */
/**   
  @author Chris Mungall
  @version @cvskw $Revision: 1.4 $
  @date @cvskw $Date: 2005/09/22 16:27:20 $
  @license @link(url='http://www.fsf.org/licensing/licenses/lgpl.html')|LGPL|

  @s1|Name| av_db - Obol atomic vocabulary database

  @s1 Synopsis
  
  @cl
  :- use_module(bio(io)).
  :- use_module(bio(av_db)).

  @/cl
  
  @s1 Description

  
  */

:- module(av_db,
          [
           adj/2,
           conj/2,
           noun/2,
           noun/3,
           prep/2,
           relational_adj_ra/3,
           relational_adj_pp/3,
           inflection/3,
           type_token/2,
           type_token/3,
           roman_numeral/3,
           greek/2,
           number/2,
           allowed_noun/2,
           
          % extensional
           ttok/2,
           relational_adj/4,
           word_ont/2,
           word_type/2,
           word_type/3,
           reserved_word/1,
           known_word/1

          ]).


/**
  @s1 Data Predicates
  
  These predicates are purely extensional - they are expected to
  come from an external data file

  @pred avnode(ID,Name,ParentID,BranchLen)
  @pred avnodeprop(ID,Key,Value)
  
  */

% metadata on the data predicates used in this module
:- use_module(bio(dbmeta)).
:- datapreds(av_db,[
                    adj/2,
                    conj/2,
                    noun/2,
                    noun/3,
                    prep/2,
                    relational_adj_ra/3,
                    relational_adj_pp/3,
                    inflection/3,
                    type_token/2,
                    type_token/3,
                    roman_numeral/3,
                    greek/2,
                    number/2
                    ]).

% --

% VIEW PREDICATES

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

word_type(X,T):- word_type(X,T,_).
word_type(X,noun,O):- anynoun(X,O).
word_type(X,adj,O):- adj(X,O).
word_type(X,prep,O):- prep(X,O).
word_type(X,ttok,O):- ttok(X,O).
word_type(X,conj,O):- conj(X,O).
word_type(X,relational_adj_pp,O):- relational_adj_pp(X,_,O).
word_type(X,relational_adj_ra,O):- relational_adj_ra(X,_,O).
word_type(X,token_symbol,builtin):- token_symbol(X).
word_type(sensu,sensu,builtin).
word_type(type,type,builtin).

word_ont(X,O):- word_type(X,_,O).

% some nouns are not part of the vocabulary, but they are
% implicitly there via relational_adjs
% ANYNOUN - any at all
noun(N,O):- noun(N,_,O).
%anynoun(N,O):- noun(N,_,O).     % right/rightmost-assoc noun
anynoun(N,O):- allowed_noun(N,O).
% right and rightmost nouns are not allowed in general contexts
allowed_noun(N,O):- noun(N,O).
allowed_noun(N,O):- inflection(_,N,O). % plurals
allowed_noun(N,O):- not(noun(N,O)),relational_adj(_,N,_,O).

token_symbol(X):- ctoken(X).

det(a).
det(the).
det(some).
det(every).
det(all).

ctoken('(').
ctoken(')').
ctoken('{').
ctoken('}').
ctoken('[').
ctoken(']').
ctoken('/').
ctoken(',').
ctoken('-').
ctoken('>').
ctoken('<').
ctoken(':').
ctoken('+').

reserved_word(sensu).
reserved_word(type).
reserved_word(class).
reserved_word(not).
known_word(X):-
        reserved_word(X)
        ;
	prep(X)
        ;
        anynoun(X,_)
        ;
        noun(X,_,_)
        ;
        adj(X,_)
        ;
        relational_adj(X,_,_,_)
        ;
	ttok(X,_)
        ;
	ttok(X,_,_)
        ;
	token_symbol(X)
        ;
        conj(X,_)
        ;
        det(X).

