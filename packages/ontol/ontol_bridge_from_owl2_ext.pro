:- module(ontol_bridge_from_owl2_ext,[]).
:- use_module(ontol_bridge_from_owl2).
:- use_module(bio(rdf_id_util),[rdfid_oboid/2]).

:- use_module(library('thea2/owl2_model')).

:- dynamic referenced/1.

:- multifile ontol_bridge_from_owl2:referenceable_hook/1.
ontol_bridge_from_owl2:referenceable_hook(X) :-
        referenced(X),
        !.
ontol_bridge_from_owl2:referenceable_hook(X) :-
        debug(owl2_ext,'testing if classExpr ~w',[X]),
        classExpression(X),
        debug(owl2_ext,' OK classExpr ~w',[X]),
        %manifestable(X),
        assert(referenced(X)).

ontol_db:class(X) :- referenced(X). % TODO: OPEs

% TODO: use ID expressions
zzzontol_db:class(X) :-
        rdfid_oboid(U,X),
        axiom(Ax),axiom_references(Ax,U),
        manifestable(U).

%ontol_db:genus(X,Y) :- class_genus_differentia(X,Y,_).
%ontol_db:differentium(X,R,Y) :- class_genus_differentia(X,_,DL),member(R=Y,DL).

zzzontol_db:class_union_element(Expr,Y) :-
        Expr=unionOf(UL),
        rdfid_oboid(UY,Y),
        axiom(Ax),axiom_references(Ax,Expr),
        \+ ((equivalent_to(Expr,C),class(C))),
        member(UY,UL).

zzzontol_db:class_intersection_element(Expr,Y) :-
        Expr=intersectionOf(UL),
        rdfid_oboid(UY,Y),
        axiom(Ax),axiom_references(Ax,Expr),
        \+ ((equivalent_to(Expr,C),class(C))),
        member(UY1,UL),
        (   owlrestriction_to_oborelationship(UY1,Y)
        ->  true
        ;   UY=UY1).



manifestable(intersectionOf(_)).
manifestable(unionOf(_)).
manifestable(complementOf(_)).


% expr_obo(intersectionOf(L),intersection_of(L2)) :-
        
