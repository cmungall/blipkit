:- module(curation_bridge_to_owl2_with_query_classes,
	  [
	   ]).

:- use_module(curation_db).
:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).

:- use_module(bio(curation_bridge_to_owl2)).
:- use_module(bio(ontol_bridge_to_owl2),[uri_oboid/2]).
:- use_module(library('thea2/owl2_model'),
	      []).

native_to_literal(Term,literal(Term)):- number(Term),!.
native_to_literal(Term,literal(lang(en,Term))).


% ----------------------------------------
% MATERIALIZE RULES
% ----------------------------------------

mat(X_URI,
    someValuesFrom(Rel_URI,GO_URI),
    [involved,in,GO_Label]) :-
        uri_oboid(GO_URI,GO),
        curation_statement(Obs,_,_,GO),
        infer_inverse_property_URI(Obs,Rel_URI),
        entity_label(GO,GO_Label),
        atom_concat(GO_URI,'-mol',X_URI).


mat(X_URI,
    someValuesFrom(Encodes_URI,
                   someValuesFrom(Rel_URI,GO_URI)),
    [encodes,molecule,involved,in,GO_Label]) :-
        rel_label(Encodes_URI,'encodes'),
        uri_oboid(GO_URI,GO),
        curation_statement(Obs,_,_,GO),
        infer_inverse_property_URI(Obs,Rel_URI),
        entity_label(GO,GO_Label),
        atom_concat(GO_URI,'-gene',X_URI).

% ----------------------------------------
% MATERIALIZE
% ----------------------------------------



owl2_model:equivalentClasses([URI,Expr]) :-
        mat(URI,Expr,_).

owl2_model:annotationAssertion('http://www.w3.org/2000/01/rdf-schema#label',URI,Literal) :-
        mat(URI,_,Toks),
        concat_atom(Toks,' ',Label),
        native_to_literal(Label,Literal).

