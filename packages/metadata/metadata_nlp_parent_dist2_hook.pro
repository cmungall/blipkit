
:- multifile metadata_nlp:parent_entity_hook/2.

metadata_nlp:parent_entity_hook(A,B) :- allowed_parent(A,B).
metadata_nlp:parent_entity_hook(A,B) :- allowed_parent(A,Z),allowed_parent(Z,B).


allowed_parent(A,B) :- subclass(A,B).
allowed_parent(A,B) :- restriction(A,part_of,B).

allowed_rel(subclass).
allowed_rel(part_of).


