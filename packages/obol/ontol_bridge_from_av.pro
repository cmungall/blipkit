/* -*- Mode: Prolog -*- */
/**
  @author Chris Mungall
  @version @cvskw $Revision: 1.1 $
  @date @cvskw $Date: 2006/09/10 21:38:55 $
  @license @link(url='http://www.fsf.org/licensing/licenses/lgpl.html')|LGPL|

  @s1|Name| ontol_bridge_from_av - masquerade av_db as an ontol_db

  @s1 Synopsis

  @cl
:- use_module(ontol_bridge_from_av).

  @/cl

  @s1 Description


 @s2 Caching

 You can cache the wordnet-as-ontology data on file, to prevent doing
the dynamic conversion every time you query an ontol_db predicate

One way to do this is to run the following on the command line:

@cl
blip -r obol_av -u ontol_bridge_from_av io-convert -to ontol_db:pro > ontol_av.pro 
@/cl

This assumes you have set up a @rp|bioresource/4| called 'obol_av',
containing a conconcatenation of all the av prolog files. You can then
set up another bioresource to point to the cached file

**/

:- module(ontol_bridge_from_av, []).

:- use_module(av_db,[]).

ontol_db:class(N,N):- av_db:word_ont(N,_).
ontol_db:belongs(ID,O):- av_db:word_ont(ID,O).
