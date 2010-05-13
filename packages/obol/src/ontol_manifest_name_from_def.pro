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


:- module(ontol_manifest_name_from_def,
          []).

:- use_module(bio(ontol_db)).
:- use_module(library(porter_stem)).

allowed_syn_type(narrow).
allowed_syn_type(broad).
allowed_syn_type(exact).
allowed_syn_type(related).
ontol_db:synonym(Class,stemmed,Stemmed):-
        restricted_class_synonym(Class,Label,_),
        split_on_comma(Label,Parts),
        maplist(stem,Parts,StemmedParts),
        concat_atom(StemmedParts,', ',Stemmed),
        Stemmed \= Label,
        debug(nlp,'stemmed ~w :: ~w -> ~w',[Class,Label,Stemmed]).

stem(Label,Stemmed):-
        atom_to_stem_list(Label,Tokens),
        concat_atom(Tokens,' ',Stemmed).

split_on_comma(Atom,Tokens):-   concat_atom(Tokens,',',Atom).

restricted_class_synonym(Class,Label,exact):- class(Class,Label).
restricted_class_synonym(Class,Label,T):-
        allowed_syn_type(T),    % avoid infinite loop
        synonym(Class,T,Label).
restricted_class_synonym(Class,Label,T):-
        allowed_syn_type(T),
        upcase_atom(T,T1),
        synonym(Class,T1,Label).
