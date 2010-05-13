/* -*- Mode: Prolog -*- */
/**
  @author Chris Mungall
  @version @cvskw $Revision: 1.2 $
  @date @cvskw $Date: 2006/08/02 07:56:25 $
  @license @link(url='http://www.fsf.org/licensing/licenses/lgpl.html')|LGPL|

  @s1|Name| ontol_manifest_name_from_def - adds synonyms by stemming each name and synonym

  @s1 Synopsis

  @cl
  :- use_module(ontol_manifest_name_from_def).

  @/cl

  @s1 Description

**/


:- module(ontol_manifest_exact_from_related_synonym,
          []).

:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).

% e.g. myeloid progenitor <-  myeloid progenitor cell
/*
ontol_db:synonym(Class,exact,Synonym):-
        entity_synonym_scope(Class,Synonym,related).
ontol_db:synonym(Class,exact,Synonym):-
        entity_synonym_scope(Class,Synonym,narrow).
*/

metadata_db:entity_synonym_scope(Class,Synonym,exact):-
        entity_synonym_scope(Class,Synonym,related).
metadata_db:entity_synonym_scope(Class,Synonym,exact):-
        entity_synonym_scope(Class,Synonym,narrow).

