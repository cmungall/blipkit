:- module(metadata_nlp).

:- multifile metadata_nlp:parent_entity_hook/2.

metadata_nlp:parent_entity_hook(A,B) :- subclassT(A,B),subclass(B,'FMA:67498').



