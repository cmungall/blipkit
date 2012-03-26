:- module(cytoscape_db,
          [edge/3,
           node/1,
           inferred_node/1,
           node_attribute_value/3,
           edge_attribute_value/3,
           edge_attribute_value/5,
           node_attribute/1,
           edge_attribute/1
           ]).

:- use_module(bio(dbmeta)).

:- extensional( node/1 ).

inferred_node(N) :-
        setof(N,
              A^V^(   node(N)
                  ;   node_attribute_value(N,A,V)),
              Ns),
        member(N,Ns).


%% edge(Source,Relation,Target).
:- extensional( edge/3 ).

%% node_attribute_value(N,A,V).
:- extensional( node_attribute_value/3 ).

%% edge_attribute_value(E,A,V), E=edge(S,R,T)
:- extensional( edge_attribute_value/3 ).

edge_attribute_value(S,R,T,A,V) :- edge_attribute_value(edge(S,R,T),A,V).

node_attribute(A) :-
        setof(A,N^V^node_attribute_value(N,A,V),As),
        member(A,As).
edge_attribute(A) :-
        setof(A,N^V^edge_attribute_value(N,A,V),As),
        member(A,As).





