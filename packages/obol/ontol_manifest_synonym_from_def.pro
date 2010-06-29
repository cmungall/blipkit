/* -*- Mode: Prolog -*- */
/**
  @author Chris Mungall
  @version @cvskw $Revision: 1.2 $
  @date @cvskw $Date: 2006/08/02 07:56:25 $
  @license @link(url='http://www.fsf.org/licensing/licenses/lgpl.html')|LGPL|

  @s1|Name| ontol_manifest_synonym_from_def - adds synonyms by stemming each name and synonym

  @s1 Synopsis

  @cl
  :- use_module(ontol_manifest_synonym_from_def).

  @/cl

  @s1 Description

**/


:- module(ontol_manifest_synonym_from_def,
          []).

:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).
:- use_module(library(porter_stem)).

metadata_db:entity_synonym(X,S):- def(X,Def),trim_def(Def,S).

trim_def(Def,Syn):-
        debug(obol,'trimming ~w',Def),
        atom_chars(Def,DefChars),
        filter_chars(DefChars,SynChars),
        atom_chars(Syn,SynChars),
        debug(obol,'trimmed: ~w => ~w',[Def,Syn]).

filter_chars([],[]).
filter_chars([C|L],[C|L2]):-
        alphanum(C),
        !,
        filter_chars(L,L2).
filter_chars(_,[]). % trim rest

alphanum(' ').
alphanum(X):- X @>= a, X @=< z.
alphanum(X):- X @>= 'A', X @=< 'Z'.
alphanum(X):- X @>= '0', X @=< '9'.

