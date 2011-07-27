/* -*- Mode: Prolog -*- */

:- module(ontol_bridge_from_dbpedia,
          [
          ]).

:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).

dbpedia(Page) :-
	setof(Page,T^(user:rdf(Page,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',T)),Pages),
	member(Page,Pages).

wpxref_url(X,URL) :-
	var(URL),!,
	atom_concat('Wikipedia:',Page,X),
	atom_concat('http://dbpedia.org/resource/',Page,URL).
wpxref_url(X,URL) :-
        atom(URL),
	atom_concat('http://dbpedia.org/resource/',Page,URL),
	atom_concat('Wikipedia:',Page,X).

ontol_db:class(X) :- dbpedia(U),wpxref_url(X,U).
ontol_db:def(X,D) :-
	user:rdf(U,'http://dbpedia.org/ontology/abstract',literal(lang(en,D))),
	wpxref_url(X,U).
ontol_db:def_xref(C,C) :-
	class(C).

% e.g. anterior spinal to vertebral
ontol_db:restriction(Post,branch_of,Pre) :-
	user:rdf(PreX,'http://dbpedia.org/property/branchFrom',PostX),
	wpxref_url(Post,PostX),
	wpxref_url(Pre,PreX).

ontol_db:restriction(Post,has_origin,Pre) :-
	user:rdf(PreX,'http://dbpedia.org/property/origin',PostX),
	wpxref_url(Post,PostX),
	wpxref_url(Pre,PreX).
ontol_db:restriction(Post,has_insertion,Pre) :-
	user:rdf(PreX,'http://dbpedia.org/property/insertion',PostX),
	wpxref_url(Post,PostX),
	wpxref_url(Pre,PreX).

% why two? ontology one may be pre-calulcated transitive closure / entailed
%  precursor = develops_from
precursor('http://dbpedia.org/ontology/precursor').
precursor('http://dbpedia.org/property/precursor').

ontol_db:restriction(Post,develops_from,Pre) :-
	user:rdf(PreX,'http://dbpedia.org/property/givesriseto',PostX),
	wpxref_url(Post,PostX),
	wpxref_url(Pre,PreX).

% links may not be asserted bidirectionally - let's be sure
ontol_db:restriction(Post,develops_from,Pre) :-
        precursor(Rel),
        user:rdf(PostX,Rel,PreX),
        wpxref_url(Post,PostX),
        wpxref_url(Pre,PreX).
         
metadata_db:entity_label(C,S) :-
	dbpedia(U),
	wpxref_url(C,U),
	wpurl_label(U,S).

metadata_db:entity_resource(C,dbpedia) :-
	dbpedia(U),
	wpxref_url(C,U).

synprop('http://dbpedia.org/property/redirect').
synprop('http://dbpedia.org/property/wikiPageRedirects').
metadata_db:entity_synonym(C,S) :-
        synprop(SynProp),
	user:rdf(SynURL,SynProp,Canonical),
	wpxref_url(C,Canonical),
	wpurl_label(SynURL,S).

wpurl_label(U,N) :-
	atom_concat('http://dbpedia.org/resource/',Page,U),
	concat_atom(Toks,'_',Page),
	concat_atom(Toks,' ',UC),
	downcase_atom(UC,N).
