/* -*- Mode: Prolog -*- */
/**
  @author Chris Mungall
  @version @cvskw $Revision: 1.2 $
  @date @cvskw $Date: 2006/07/30 02:33:30 $
  @license @link(url='http://www.fsf.org/licensing/licenses/lgpl.html')|LGPL|

  @s1|Name| ontol_bridge_from_wn - masquerade wn_db as an ontol_db

  @s1 Synopsis

  @cl
:- use_module(ontol_bridge_from_wn).

  @/cl

  @s1 Description

  The obol ontol_db and wn_db modules are quite similar. This module
allows a wordnet database to be used as if it were an ontology conforming to the ontol_db datalog model (eg see @rp|class/2|, @rp|subclass/2|)

 @s2 Mapping

 @list 
 @li @rp|class/2| is a view over @rp|s/6| in wn_db
    the name is arbitrarily chosen to be word_num=1
 @li @rp|synonym/3| is a view over @rp|s/6| in wn_db
 @li @rp|belongs/2| is a view over @rp|s/6| in wn_db
    wordnet is artifically partitioned into one 'ontology'
    per ss_type (eg a "noub" ontology...)
 @li @rp|subclass/2| is a view over @rp|hyp/2| in wn_db
 @li @rp|restriction/3| is a view over @rp|mm/2|, @rp|mp/2|, @rp|ms/2| in wn_db
 @/list

 @s2 Caching

 You can cache the wordnet-as-ontology data on file, to prevent doing
the dynamic conversion every time you query an ontol_db predicate

One way to do this is to run the following on the command line:

@cl
blip -r wn -u ontol_bridge_from_wn io-convert -to pro > ontol_wn.pro 
@/cl

This assumes you have set up a @rp|bioresource/4| called 'wn',
containing a conconcatenation of all the wn prolog files. You can then
set up another bioresource to point to the cached file

**/

:- module(ontol_bridge_from_wn, []).

:- use_module(wn_db,[]).

ontol_db:class(ID,N):- wn_db:s(ID,1,N,_,_,_).
ontol_db:synonym(ID,related,N):- wn_db:s(ID,_,N,_,_,_).
ontol_db:subclass(ID,PID):- wn_db:hyp(ID,PID).
ontol_db:belongs(ID,O):- wn_db:s(ID,_,_,O,_,_).
ontol_db:restriction(ID,part_of,PID):- wn_db:mp(ID,PID).
ontol_db:restriction(ID,member_part_of,PID):- wn_db:mm(ID,PID).
ontol_db:restriction(ID,substance_part_of,PID):- wn_db:ms(ID,PID).
ontol_db:restriction(ID,morphologically_derived_from,PID):- wn_db:der(ID,PID).


