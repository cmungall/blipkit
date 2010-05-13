:- module(nodeq,[]).

:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).

subclassOf(X,To):- subclass(X,To).
superclassOf(X,To):- subclass(To,X).
transitiveSuperclassOf(X,To):- subclassT(To,X).
transitiveSubclassOf(X,To):- subclassT(X,To).
linkTo(link(R,X),To):- node_link(X,R,To).
linkFrom(link(R,To),X):- node_link(To,R,X).
label(X,Y):- entity_label(X,Y).
synonym(X,Y):- entity_synonym(X,Y).
node(link(_,X),X).
relation(link(X,_),X).
foo(X,X):- class(X).
isClass(X,X):- class(X).

blipfun:unary(class).
blifun:unary(property).



