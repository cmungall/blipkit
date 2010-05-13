:- module(owl_util,
          [
           rdf_literal_to_native/2
          ]).

rdf_literal_to_native(X,Y):-
        rdf_literal_to_native1(X,Y),
        atom(Y).
rdf_literal_to_native1(literal(lang(en,X)),X):- !.
rdf_literal_to_native1(literal(lang(_,X)),X):- !.
rdf_literal_to_native1(literal(type(_,X)),X):- !.
rdf_literal_to_native1(literal(X),X):- !.
rdf_literal_to_native1(X,X):- !.

