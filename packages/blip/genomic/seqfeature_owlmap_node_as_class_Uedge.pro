:- module(seqfeature_owlmap_node_as_class_Uedge,
          [
          ]).

:- use_module(bio(seqfeature_db)).
:- use_module(bio(seqfeature_owlmap_node_as_class_basic)).
:- use_module(bio('thea/owl_parser')).

owl_parser:subclassOf(X,restriction(Rel,allValuesFrom(Y))):- seqfeature_db:feature_relationship(X,Y,Rel,_).
