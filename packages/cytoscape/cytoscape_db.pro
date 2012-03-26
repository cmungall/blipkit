:- module(cytoscape_model,
          [edge/3,
           node/1,
           node_attribute_value/3,
           edge_attribute_value/3,
           edge_attribute_value/5,
           node_attribute/1,
           edge_attribute/1,
           ]).

:- multifile node/1.

%% edge(Source,Relation,Target).
:- multifile edge/3.

%% node_attribute_value(N,A,V).
:- multifile node_attribute_value/3.

%% edge_attribute_value(E,A,V), E=edge(S,R,T)
:- multifile edge_attribute_value/3.
edge_attribute_value(S,R,T,A,V) :- edge_attribute_value(edge(S,R,T),A,V).

node_attribute(A) :-
        setof(A,N^V^node_attribute_value(N,A,V),As),
        member(A,As).
edge_attribute(A) :-
        setof(A,N^V^edge_attribute_value(N,A,V),As),
        member(A,As).





