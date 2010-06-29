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
:- use_module(library(porter_stem)).

allowed_syn_type(narrow).
allowed_syn_type(broad).
allowed_syn_type(exact).
allowed_syn_type(related).
stemmed_synonym(Class,Stemmed):-
        restricted_class_synonym(Class,Label,_),
        atom_to_stem_list(Label,Stems),
        concat_atom(Stems,' ',Stemmed),
        Stemmed \= Label,
        debug(nlp,'stemmed ~w :: ~w -> ~w',[Class,Label,Stemmed]).
stemmed_synonym(Class,Stemmed):-
        restricted_class_synonym(Class,Label,_),
        tokenize_atom(Label,Stems),
        concat_atom(Stems,' ',Stemmed),
        Stemmed \= Label,
        debug(nlp,'exact ~w :: ~w -> ~w',[Class,Label,Stemmed]).
metadata_db:entity_synonym(E,S):- stemmed_synonym(E,S).
metadata_db:entity_synonym_scope(E,S,stemmed):- stemmed_synonym(E,S).


restricted_class_synonym(Class,Label,exact):- class(Class,Label).
restricted_class_synonym(Class,Label,T):-
        allowed_syn_type(T),    % avoid infinite loop
        synonym(Class,T,Label).
restricted_class_synonym(Class,Label,T):-
        allowed_syn_type(T),
        upcase_atom(T,T1),
        synonym(Class,T1,Label).
