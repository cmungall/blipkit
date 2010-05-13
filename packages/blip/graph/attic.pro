graph_map(graph(G,Atts,Elts),graph(GX,Atts,EltsX),IDMap):-
        id_map(G,GX,IDMap),
        graph_map(Elts,EltsX,IDMap).
graph_map(subgraph(G,Atts,Elts),subgraph(GX,Atts,EltsX),IDMap):-
        id_map(G,GX,IDMap),
        graph_map(Elts,EltsX,IDMap).
graph_map(node(N,Atts),node(NX,Atts),IDMap):-
        id_map(N,GX,IDMap).
graph_map(edge(A,B,Atts),edge(AX,BX,Atts),IDMap):-
        id_map(A,AX,IDMap),
        id_map(B,BX,IDMap).
graph_map([],[],_).
graph_map([H|T],[HX|TX],IDMap):-
        graph_map(H,HX,IDMap),
        graph_map(T,TX,IDMap).
graph_map(X,X,_).

id_map(A,AX,IDMap):- member(A-AX,IDMap),!.
id_map(A,A,_).

        
%element(graph(G,Atts,Elts),[Atts,Elts],
graph_map(G,GX,IDMap):-
        G =.. [E,N|L],
        (   E=graph;E=subgraph),
        !,
        id_map(N,NX),
        graph_map(L,LX,IDMap),
        GX =.. [E,NX|LX].

graph_map(N,NX,IDMap):-
        N =.. [node,ID|L],
        !,
        id_map(ID,IDX),
        graph_map(L,LX,IDMap),
        N =.. [node,IDX|LX].
