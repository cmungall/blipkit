/* -*- Mode: Prolog -*- */


:- module(ontol_manifest_metadata_from_bfo,
          []).

:- use_module(bio(ontol_bridge_from_owl)).
:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).
:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdfs')).

%:- rdf_register_ns(bfo,'http://www.ifomis.org/bfo/1.0#').
%:- rdf_register_ns(snap,'http://www.ifomis.org/bfo/1.0/snap#').
%:- rdf_register_ns(span,'http://www.ifomis.org/bfo/1.0/span#').

:- rdf_register_ns(bfo,'http://www.ifomis.org/bfo/1.1#',[force(true)]).
:- rdf_register_ns(snap,'http://www.ifomis.org/bfo/1.1/snap#',[force(true)]).
:- rdf_register_ns(span,'http://www.ifomis.org/bfo/1.1/span#',[force(true)]).

:- rdf_register_ns(bfo1_0,'http://www.ifomis.org/bfo/1.0#',[force(true)]).
:- rdf_register_ns(snap1_0,'http://www.ifomis.org/bfo/1.0/snap#',[force(true)]).
:- rdf_register_ns(span1_0,'http://www.ifomis.org/bfo/1.0/span#',[force(true)]).


% do not use rdfs:comment directly
:- abolish(ontol_db:class_comment/2).
ontol_db:class_comment(Class,Comment):-
        entity_comment(Class,Comment1),
        parse_tagval(comment,Comment1,Comment).

ontol_db:def(Class,Def):-
        entity_comment(Class,Comment),
        parse_tagval(def,Comment,Def).

metadata_db:entity_synonym(Class,Syn):-
        entity_comment(Class,Comment),
        parse_tagval(synonyms,Comment,Syns),
        member(Syn,Syns).

ontol_db:inst_sv(Class,has_example,Example,'xsd:string'):-
        entity_comment(Class,Comment),
        parse_tagval(examples,Comment,Examples),
        member(Example,Examples).

parse_tagval(def,Field,Val):-       parse_tagval2('Definition',Field,Val).
parse_tagval(comment,Field,Val):-       parse_tagval2('Comment',Field,Val).
parse_tagval(synonyms,Field,L):-       parse_tagval2('Synonyms',Field,Val),concat_atom(L,', ',Val).
parse_tagval(examples,Field,L):-       parse_tagval2('Examples',Field,Val),concat_atom(L,', ',Val).

parse_tagval2(Tag,Field,Val):-
        atom_concat(Tag,': ',Tag2),
        atom_concat(Tag2,Val,Field).

%ontol_db:synonym(Class,exact,Syn):-
%        class(Class,N),
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

**/
