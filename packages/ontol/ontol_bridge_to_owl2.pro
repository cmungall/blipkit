/* -*- Mode: Prolog -*- */

:- module(ontol_bridge_to_owl2,[
                                uri_oboid/2
                               ]).
:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).
:- use_module(library('thea2/owl2_model'),
	      []).

native_to_literal(Term,literal(Term)):- number(Term),!.
native_to_literal(Term,literal(lang(en,Term))).

% ----------------------------------------
% ONTOLOGY
% ----------------------------------------

% we don't assert ontologyAxiom/2

owl2_model:ontology(UO) :-
        ontology(UO),
        sub_atom(UO,0,_,_,'http').
owl2_model:ontology(UO) :-
        ontology(O),
        concat_atom(['http://purl.obolibrary.org/obo/',O],UO).

% ----------------------------------------
% METADATA
% ----------------------------------------

% only convert a minimal amount - leave the rest to bridge modules

owl2_model:annotationAssertion('http://www.w3.org/2000/01/rdf-schema#label',UA,UX) :-
        uri_oboid(UA,A),
        entity_label(A,X),
        native_to_literal(X,UX).

owl2_model:annotationAssertion('http://purl.obolibrary.org/obo/IAO_0000424',UA,UX) :-
        uri_oboid(UA,A),
        expand_expression_to(A,X),
        native_to_literal(X,UX).

owl2_model:annotationAssertion('http://purl.obolibrary.org/obo/IAO_0000425',UA,UX) :-
        uri_oboid(UA,A),
        expand_assertion_to(A,X),
        native_to_literal(X,UX).

owl2_model:annotationProperty('http://purl.obolibrary.org/obo/IAO_0000425') :- \+ \+ expand_assertion_to(_,_).
owl2_model:annotationProperty('http://purl.obolibrary.org/obo/IAO_0000424') :- \+ \+ expand_expression_to(_,_).



% ----------------------------------------
% DECLARATIONS
% ----------------------------------------

% obsoletes?

owl2_model:class(UA) :- uri_oboid(UA,A),class(A).
owl2_model:objectProperty(UA) :- uri_oboid(UA,A),property(A),\+is_class_level(A).
owl2_model:annotationProperty(UA) :- uri_oboid(UA,A),property(A),is_class_level(A).
owl2_model:namedIndividual(UA) :- uri_oboid(UA,A),inst(A).

% ----------------------------------------
% PROPERTIES
% ----------------------------------------

owl2_model:transitiveProperty(UA) :- uri_oboid(UA,A),is_transitive(A),\+is_class_level(A).
owl2_model:reflexiveProperty(UA) :- uri_oboid(UA,A),is_reflexive(A),\+is_class_level(A).
owl2_model:symmetricProperty(UA) :- uri_oboid(UA,A),is_symmetric(A),\+is_class_level(A).
owl2_model:asymmetricProperty(UA) :- uri_oboid(UA,A),is_asymmetric(A),\+is_class_level(A).

owl2_model:functionalProperty(UA) :- uri_oboid(UA,A),is_functional(A),\+is_class_level(A).
owl2_model:inverseFunctionalProperty(UA) :- uri_oboid(UA,A),is_inverse_functional(A),\+is_class_level(A).

owl2_model:inverseProperties(UA,UB) :- uri_oboid(UA,A),uri_oboid(UB,B),inverse_of(A,B).
owl2_model:subPropertyOf(propertyChain([UA,UB]),UA) :- uri_oboid(UA,A),uri_oboid(UB,B),transitive_over(A,B).
owl2_model:subPropertyOf(propertyChain([UA,UB]),UC) :- uri_oboid(UA,A),uri_oboid(UB,B),uri_oboid(UC,C),holds_over_chain(C,[A,B]).
owl2_model:subPropertyOf(propertyChain([UA,UB]),UC) :- uri_oboid(UA,A),uri_oboid(UB,B),uri_oboid(UC,C),equivalent_to_chain(C,[A,B]).

owl2_model:propertyDomain(UA,UB) :- uri_oboid(UA,A),uri_oboid(UB,B),property_domain(A,B).
owl2_model:propertyRange(UA,UB) :- uri_oboid(UA,A),uri_oboid(UB,B),property_range(A,B).

owl2_model:subPropertyOf(UA,UB) :-
	uri_oboid(UA,A),uri_oboid(UB,B),
        subclass(A,B),
        property(A).

/*
owl2_model:disjointProperties([UA,UB]) :-
	uri_oboid(UA,A),uri_oboid(UB,B),
        disjoint_from(A,B),
        property(A),
        property(B).
*/

% ----------------------------------------
% CLASS AXIOMS
% ----------------------------------------

owl2_model:subClassOf(UA,UB) :-
	uri_oboid(UA,A),uri_oboid(UB,B),
        subclass(A,B),
        class(A).
owl2_model:subClassOf(UA,Expr):-
	uri_oboid(UA,A),
	restriction(A,P,B),
	pval_expr(P,B,Expr).

owl2_model:equivalentClasses([UA,UB]) :-
	uri_oboid(UA,A),
	uri_oboid(UB,B),
        equivalent_class(A,B).

owl2_model:equivalentClasses([UA,intersectionOf([UG|Restrs])]) :-
	uri_oboid(UA,A),
	uri_oboid(UG,G),
        genus(A,G),
        findall(Restr,(
                       differentium(A,P,B),
                       pval_expr(P,B,Restr)),
                Restrs).


owl2_model:disjointUnion(UA,UL) :-
        uri_oboid(UA,A),
        class_disjoint_union_list(A,L),
        findall(UX,(member(X,L),
                    uri_oboid(UX,X)),
                UL).

owl2_model:disjointClasses([UA,UB]) :-
	uri_oboid(UA,A),uri_oboid(UB,B),
        disjoint_from(A,B),
        class(A),
        class(B).

owl2_model:disjointClasses( [ someValuesFrom(UQ,UA), someValuesFrom(UQ,UB) ] ) :-
	uri_oboid(UA,A),uri_oboid(UB,B),
        uri_oboid(UQ,Q),
        disjoint_over(P,Q),
        restriction(A,P,B).

% ----------------------------------------
% INSTANCE AXIOMS
% ----------------------------------------

owl2_model:classAssertion(UA,UB) :-
	uri_oboid(UA,A),uri_oboid(UB,B),
        inst_of(B,A).

owl2_model:propertyAssertion(UP,UA,UB) :-
	uri_oboid(UA,A),uri_oboid(UB,B),
        uri_oboid(UP,P),
        inst_rel(A,P,B).

% inst_sv/4 TODO

% ----------------------------------------
% CLASS EXPRESSIONS
% ----------------------------------------

% the default is an existential restriction. obo also allows
% cardinality, but care must be taken with transitive properties
% (OWL-DL does not allow QCRs on transitive properties).

pval_expr(card(P,_,0 ),B,allValuesFrom(UP,complementOf(UB))) :- 
	uri_oboid(UB,B),
	uri_oboid(UP,P),
        !.
pval_expr(card(P,N),B,minCardinality(N,UP,UB)) :- 
	uri_oboid(UB,B),
	uri_oboid(UP,P),
        \+ is_transitive(P),
        !.
pval_expr(card(P,N,N),B,exactCardinality(N,UP,UB)) :- 
	uri_oboid(UB,B),
	uri_oboid(UP,P),
        \+ is_transitive(P),
        !.
pval_expr(card(P,Min,Max),B,intersectionOf([minCardinality(Min,UP,UB),maxCardinality(Max,UP,UB)])) :- 
	uri_oboid(UB,B),
	uri_oboid(UP,P),
        \+ is_transitive(P),
        !.
pval_expr(card(P,_),B,someValuesFrom(UP,UB)) :- % no cardinality on transitive in OWL-DL
	uri_oboid(UB,B),
	uri_oboid(UP,P),
        is_transitive(P),
        !.
pval_expr(card(P,_,_),B,someValuesFrom(UP,UB)) :- % no cardinality on transitive in OWL-DL
	uri_oboid(UB,B),
	uri_oboid(UP,P),
        is_transitive(P),
        !.
pval_expr(P,B,someValuesFrom(UP,UB)) :- % GUESS existential
	uri_oboid(UB,B),
	uri_oboid(UP,P),
        \+ is_class_level(P),
        !.
pval_expr(P,B,hasValue(UP,UB)) :- % GUESS existential
	uri_oboid(UB,B),
	uri_oboid(UP,P),
        is_class_level(P),
        !.
pval_expr(R,To,_) :- throw(pval_expr('cannot translate ~w ~w',[R,To])).


%% uri_oboid(?U,+X)
uri_oboid(U,X) :-
        var(X),
        !,
        freeze(X,uri_oboid(U,X)).
uri_oboid(U,X) :-
        concat_atom([S,A],':',X),
        !,
        concat_atom(['http://purl.obolibrary.org/obo/',S,'_',A],U).
uri_oboid(U,X) :-
        concat_atom([S|L],':',X), % > 1 separator
        L\=[],
        !,
        concat_atom(['http://purl.obolibrary.org/obo/',S,'_'|L],U).
uri_oboid(U,X) :-
        % hacky translation of relation URIs
        property(X),
        entity_xref(X,Y),
        id_idspace(Y,S),
        (   S='RO'
        ;   S='BFO'),
        !,
        uri_oboid(U,Y).
uri_oboid(U,X) :-
        !,
        concat_atom(['http://purl.obolibrary.org/obo/',X],U).

