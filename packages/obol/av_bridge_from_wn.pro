/* -*- Mode: Prolog -*- */
/**
  @author Chris Mungall
  @version @cvskw $Revision: 1.2 $
  @date @cvskw $Date: 2005/09/28 22:01:01 $
  @license @link(url='http://www.fsf.org/licensing/licenses/lgpl.html')|LGPL|

  @s1|Name| av_bridge_from_wn - masquerade wn_db as an av_db

  @s1 Synopsis

  @cl
:- use_module(av_bridge_from_wn).

  @/cl

  @s1 Description

  The obol av_db and wn_db modules are quite similar. This module
allows a wordnet database to be used as if it were an obol AV (atomic
vocab) database.

  The wn_db @rp|s/6| data predicate is used to answer queries from the
av_db @rp|adj/2|, @rp|noun/2| etc predicates

  av_db expects words to be partitioned into domains (corresponding to
upper ontology classes). Should we determin these automatically based
on wordnet hyponyms????

Some relevenant roots from wn:

activity
event
act
physical_object


**/


:- module(av_bridge_from_wn, []).

:- use_module(wn_db,[]).

av_db:noun(W,wn):- wn_db:s(_,_,W,n,_,_).
av_db:adj(W,wn):- wn_db:s(_,_,W,a,_,_).
av_db:adj(W,wn):- wn_db:s(_,_,W,s,_,_). % satellite adj

