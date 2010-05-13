/* -*- Mode: Prolog -*- */
/**
  @author Chris Mungall
  @version @cvskw $Revision: 1.2 $
  @date @cvskw $Date: 2007/01/21 02:31:24 $
  @license @link(url='http://www.fsf.org/licensing/licenses/lgpl.html')|LGPL|

  @s1|Name| fma_synonym_view - adds synonyms by stemming each name and synonym

  @s1 Synopsis

  @cl
  :- use_module(fma_synonym_view).

  @/cl

  @s1 Description

  stems class names and presents them as synonym/3 (type stemmed)

  TODO: fix duplication with disease_ontology_synonym_view
  
  @cl
  
  @/cl
  
**/


:- module(fma_synonym_view,
          []).

:- use_module(bio(metadata_db)).
:- use_module(bio(ontol_db)).
:- use_module(bio(mode)).

stemmed_synonym(Class,Stemmed):-
        class(Class,N),
        downcase_atom(N,Stemmed).
metadata_db:entity_synonym_scope(E,S,exact):- stemmed_synonym(E,S).
metadata_db:entity_synonym(E,S):- stemmed_synonym(E,S).
