/* -*- Mode: Prolog -*- */

:- module(ontol_bridge_from_dbpedia,
          [
          ]).

:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).

dbpedia(Page) :-
	setof(Page,T^(user:rdf(Page,'http://xmlns.com/foaf/0.1/isPrimaryTopicOf',T)),Pages),
	member(Page,Pages).

%dbpedia(Page) :-
%	setof(Page,T^(user:rdf(Page,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',T)),Pages),
%	member(Page,Pages).

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


ontol_db:subclass(Sub,Sup) :-
	user:rdf(SubU,'http://purl.org/dc/terms/subject',SupU),
        atom_concat('http://dbpedia.org/resource/Category:',Cat,SupU),
        atom_concat('WikipediaCategory:',Cat,Sup),
        %wpxref_url(Sup,SupU),
        wpxref_url(Sub,SubU).


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

ontol_db:restriction(P,part_of,W) :-
	user:rdf(PX,'http://dbpedia.org/property/ispartof',WX),
	wpxref_url(P,PX),
	wpxref_url(W,WX).

metadata_db:entity_label(C,S) :-
	dbpedia(U),
	wpxref_url(C,U),
	wpurl_label(U,S).

% todo - use depiction property
metadata_db:entity_xref(C,X) :-
	user:rdf(U,'http://xmlns.com/foaf/0.1/depiction',X),
	wpxref_url(C,U).

metadata_db:entity_resource(C,dbpedia) :-
	dbpedia(U),
	wpxref_url(C,U).

metadata_db:entity_partition(C,Cat) :-
	user:rdf(U,'http://purl.org/dc/terms/subject',CatURL),
        atom_concat('http://dbpedia.org/resource/Category:',Cat,CatURL),
        atom_concat(_,'_anatomy',Cat),
        wpxref_url(C,U).

synprop('http://dbpedia.org/property/redirect').
synprop('http://dbpedia.org/property/wikiPageRedirects').
metadata_db:entity_synonym(C,S) :-
        synprop(SynProp),
	user:rdf(SynURL,SynProp,Canonical),
	wpxref_url(C,Canonical),
	wpurl_label(SynURL,S).

% e.g. club (anatomy)
metadata_db:entity_synonym(C,S) :-
        metadata_db:entity_label(C,Label),
        atomic_list_concat([S,_],' (',Label).


latin(X,N) :-
	user:rdf(U,'http://dbpedia.org/property/latin',literal(lang(en,N))),
	wpxref_url(X,U).

metadata_db:entity_synonym(X,S) :-
        latin(X,S).
metadata_db:entity_synonym_type(X,S,'LATIN') :-
        latin(X,S).
metadata_db:entity_synonym_scope(X,S,'EXACT') :-
        latin(X,S).


wpurl_label(U,N) :-
	atom_concat('http://dbpedia.org/resource/',Page,U),
	concat_atom(Toks,'_',Page),
	concat_atom(Toks,' ',UC),
	downcase_atom(UC,N).
