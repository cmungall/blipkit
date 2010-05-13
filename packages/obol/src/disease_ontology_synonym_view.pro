/* -*- Mode: Prolog -*- */
/**
  @author Chris Mungall
  @version @cvskw $Revision: 1.1 $
  @date @cvskw $Date: 2006/09/10 21:38:55 $
  @license @link(url='http://www.fsf.org/licensing/licenses/lgpl.html')|LGPL|

  @s1|Name| disease_ontology_synonym_view - adds synonyms by stemming each name and synonym

  @s1 Synopsis

  @cl
  :- use_module(disease_ontology_synonym_view).

  @/cl

  @s1 Description

  stems class names and presents them as synonym/3 (type stemmed)

  
  @cl
  
  @/cl
  
**/


:- module(disease_ontology_synonym_view,
          []).

:- use_module(bio(ontol_db)).
:- use_module(bio(mode)).
:- use_module(library(porter_stem)).

allowed_syn_type(narrow).
allowed_syn_type(broad).
allowed_syn_type(exact).
allowed_syn_type(related).
ontol_db:synonym(Class,stemmed,Stemmed):-
        restricted_class_synonym(Class,Label,_),
        split_on_comma(Label,Parts),
        maplist(stem_phrase,Parts,StemmedParts),
        concat_atom(StemmedParts,', ',Stemmed),
        Stemmed \= Label,
        debug(nlp,'stemmed ~w :: ~w -> ~w',[Class,Label,Stemmed]).

stem_phrase(Phrase,Stemmed):-
        split_on_ws(Phrase,Words),
        maplist(stem,Words,Words2),
        concat_atom(Words2,' ',Stemmed).

:- mode stem(+,?) is nondet.   
stem(Label,Stemmed):-
        downcase_atom(Label,LabelDn),
        remove_s(LabelDn,Stemmed).

% succeeds multiple times for different combos with and without 's' at end
:- mode remove_s(+,?) is nondet.
remove_s(Label,Label).
remove_s(Label,Label2):-
        atom_concat(Stem,ies,Label),
        !,
        atom_concat(Stem,y,Label2).
remove_s(Label,Stem):-
        atom_concat(Stem,s,Label).
        %sub_atom(Label,Pos,_,0,s),
        %sub_atom(Label,0,_,1,Label2).

split_on_ws(Atom,Tokens):-   concat_atom(Tokens,' ',Atom).
split_on_comma(Atom,Tokens):-   concat_atom(Tokens,',',Atom).

restricted_class_synonym(Class,Label,exact):- class(Class,Label).
restricted_class_synonym(Class,Label,T):-
        allowed_syn_type(T),    % avoid infinite loop
        synonym(Class,T,Label).
restricted_class_synonym(Class,Label,T):-
        allowed_syn_type(T),
        upcase_atom(T,T1),
        synonym(Class,T1,Label).
