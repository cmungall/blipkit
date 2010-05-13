:- module(seqfeature_owlmap_node_as_individual,
          [
          ]).

:- use_module(bio(seqfeature_db)).
:- use_module(bio('thea/owl_parser')).

:- multifile fact/2.

fact(F,value(Rel,To)):-
        seqfeature_db:feature_relationship(F,To,Rel,_).

owl_parser:individual(F,[],[T],Facts):- seqfeature_db:feature_type(F,T),findall(Fact,fact(F,Fact),Facts).
owl_parser:differentIndividuals(L):- findall(F,seqfeature_db:feature(F),L).
