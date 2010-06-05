/* -*- Mode: Prolog -*- */



:- module(ontol_bridge_from_fmaowl,
          []).

:- use_module(bio(ontol_bridge_from_owl)).
:- use_module(bio(ontol_db)).
:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdfs')).
%:- use_module(library(porter_stem)).

ontol_db:def(ID,Def):-
        rdf_has(ID,
                'http://nlm.nih.gov/ontology/FMAInOWL#definition',
                literal(lang(en,N))). % TODO: lang prefs

%ontol_db:synonym(ID,exact,Syn):-
%        class(ID,N),
%        tokenize_atom(N,Toks),
        
/** <module>
  @author Chris Mungall
  @version  $Revision$
  @date  $Date$
  @license LGPL

  ---+ Name
  ---++ ontol_bridge_from_fmaowl
- 

  ---+ Synopsis

  ==
  :- use_module(bio(ontol_bridge_from_fmaowl)).

  ==

  ---+ Description

  use:
  ontol_bridge_from_owlfull_fma
**/