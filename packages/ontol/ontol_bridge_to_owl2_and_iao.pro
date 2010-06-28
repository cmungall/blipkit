/* -*- Mode: Prolog -*- */

:- module(ontol_bridge_to_owl2_and_iao,
          [
           ]).

:- use_module(ontol_bridge_to_owl2).
:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).
:- use_module(library('thea2/owl2_model'),
	      []).

native_to_literal(Term,literal(Term)):- number(Term),!.
native_to_literal(Term,literal(lang(en,Term))).

owl2_model:annotationProperty('http://purl.obolibrary.org/obo/IAO_0000115').
owl2_model:annotationProperty('http://purl.obolibrary.org/obo/IAO_0000118').
owl2_model:annotationProperty('http://purl.obolibrary.org/obo/IAO_0000119').
owl2_model:annotationProperty('http://purl.org/dc/elements/1.1/source').

owl2_model:annotationAssertion('http://purl.obolibrary.org/obo/IAO_0000115',UA,UX) :-
        uri_oboid(UA,A),
        def(A,X),
        native_to_literal(X,UX).

owl2_model:annotationAssertion('http://purl.obolibrary.org/obo/IAO_0000119',UA,UX) :-
        uri_oboid(UA,A),
        def_xref(A,X),
        native_to_literal(X,UX).

owl2_model:annotationAssertion('http://purl.obolibrary.org/obo/IAO_0000118',UA,UX) :-
        uri_oboid(UA,A),
        entity_synonym(A,X),
        native_to_literal(X,UX).

/*
owl2_model:annotation( annotationAssertion('http://purl.obolibrary.org/obo/IAO_0000118',UA,UX), 'http://purl.org/dc/elements/1.1/source', US ) :-
        uri_oboid(UA,A),
        entity_synonym(A,X),
        native_to_literal(X,UX),
        entity_synonym_xref(A,X,S),
        native_to_literal(S,US).
*/



%owl2_model:annotationAssertion('http://purl.obolibrary.org/obo/IAO_0000427',UA,literal(true)) :-
%        uri_oboid(UA,A),is_anti_symmetric(A),\+is_class_level(A).


/*
IAO_0000002     example to be eventually removed
IAO_0000027     data item
IAO_0000030     information content entity
IAO_0000078     curation status specification
IAO_0000102     data about an ontology part
IAO_0000103     failed exploratory term
IAO_0000111     editor preferred term
IAO_0000112     example of usage
IAO_0000113     in branch
IAO_0000114     has curation status
IAO_0000115     definition
IAO_0000116     editor note
IAO_0000117     definition editor
IAO_0000118     alternative term
IAO_0000119     definition source
IAO_0000120     metadata complete
IAO_0000121     placeholder
IAO_0000122     ready for release
IAO_0000123     metadata incomplete
IAO_0000124     uncurated
IAO_0000125     pending final vetting
IAO_0000224     core
IAO_0000225     obsolescence reason specification
IAO_0000226     placeholder removed
IAO_0000227     terms merged
IAO_0000228     term imported
IAO_0000229     term split
IAO_0000230     other
IAO_0000231     has obsolescence reason
IAO_0000232     curator note
IAO_0000409     denotator type
IAO_0000410     universal
IAO_0000411     is denotator type
IAO_0000412     imported from
IAO_0000420     defined class
IAO_0000421     named class expression
IAO_0000424     expand expression to
IAO_0000425     expand assertion to
IAO_0000426     first order logic expression
IAO_0000427     antisymmetric property
IAO_0100001     term replaced by
*/
