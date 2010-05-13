:- module(seqfeature_owlmap_node_as_class_basic,
          [
          ]).

:- use_module(bio(seqfeature_db)).
:- use_module(bio('thea/owl_parser')).
:- use_module(library('semweb/rdf_db')).

owl_parser:class(F,no,_,[],nil):- seqfeature_db:feature(F).
owl_parser:subclassOf(F,T):- seqfeature_db:feature_type(F,T).
owl_parser:disjointSet(L):- setof(F,seqfeature_db:feature(F),L).
