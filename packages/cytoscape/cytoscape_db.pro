:- module(cytoscape_db,
          [edge/3,
           node/1,
           inferred_node/1,
           node_attribute_value/3,
           edge_attribute_value/3,
           edge_attribute_value/5,
           node_attribute/1,
           edge_attribute/1,
           cytoscape_tblmap/1,
           collapse/2
           ]).

:- use_module(bio(dbmeta)).
:- use_module(bio(io)).

:- extensional( node/1 ).

inferred_node(N) :-
        setof(N,
              A^V^(   node(N)
                  ;   node_attribute_value(N,A,V)),
              Ns),
        member(N,Ns).


%% edge(Source,Relation,Target).
:- dynamic( edge/3 ).
:- extensional( edge/3 ).

%% node_attribute_value(N,A,V).
:- extensional( node_attribute_value/3 ).

%% edge_attribute_value(E,A,V), E=edge(S,R,T)
:- dynamic( edge_attribute_value/3 ).
:- extensional( edge_attribute_value/3 ).

edge_attribute_value(S,R,T,A,V) :- edge_attribute_value(edge(S,R,T),A,V).

node_attribute(A) :-
        setof(A,N^V^node_attribute_value(N,A,V),As),
        member(A,As).
edge_attribute(A) :-
        setof(A,N^V^edge_attribute_value(N,A,V),As),
        member(A,As).

cytoscape_tblmap(File) :-
        retractall(tmptbl(_,_,_)),
        load_biofile(tbl('user:tmptbl'),File),
        forall(user:tmptbl(X,Y,_),
               assert(edge(X,connected,Y))),
        forall(user:tmptbl(X,Y,W),
               assert(edge_attribute_value(edge(X,connected,Y),weight,W))).

               
collapse(Type,edge(N1,link,N2)) :-
        node_attribute_value(Nx,type,Type),
        edge(N1,_,Nx),
        edge(N2,_,Nx),
        N1 \= N2.
collapse(Type,Fact) :-
        Fact=node_attribute_value(N,_,_),
        Fact,
        \+ node_attribute_value(N,Type).
collapse(Type,node(N)) :-
        node(N),
        \+ node_attribute_value(N,Type).

        
                



