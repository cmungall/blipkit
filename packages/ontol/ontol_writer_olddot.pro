/* -*- Mode: Prolog -*- */

% DEPRECATED??

:- module(ontol_writer_dot,[
                            write_dot/1
                           ]).
:- use_module(bio(ontol_db)).
:- use_module(bio(graphviz)).

% TODO - stdout
io:write_all(ontol_dot,F,_):-
        graphterm(G),
        graph_to_file(G,dot,F).
        
write_dot(F):-
        graphterm(G),
        graph_to_file(G,dot,F).

graphterm(graph(foo,SubTerms)):-
        Style=[prop(style=filled)],
        findall(node(ID,[label=N,fillcolor=red,shape=box]),
                entity_label(ID,N),
                ClassNodes),
        findall(node(ID,[label=N,fillcolor=red,shape=box]),
                inst(ID,N),
                InstNodes),
        findall(edge(ID,PID,[label=is_a,weight=100]),
                subclass(ID,PID),
                IsAEdges),
        findall(edge(ID,PID,[label=T,weight=20]),
                restriction(ID,T,PID),
                RestrictionEdges),
        findall(edge(ID,PID,[label=inst_of,weight=100]),
                inst_of(ID,PID),
                InstOfEdges),
        findall(edge(ID,PID,[label=T,weight=20]),
                inst_rel(ID,T,PID),
                InstRelEdges),
        findall(edge(ID,PID,[label=T,weight=20]),
                reification(ID,restriction(PID,_,_)),
                ReifEdges),
        flatten([Style,ClassNodes,InstNodes,IsAEdges,RestrictionEdges,InstOfEdges,InstRelEdges,ReifEdges],
                SubTerms).


/** <module>   
  @author Chris Mungall
  @version  $Revision: 1.2 $
  @date  $Date: 2006/03/18 04:00:47 $
  @license LGPL

  */
