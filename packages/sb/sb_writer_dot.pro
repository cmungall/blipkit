/* -*- Mode: Prolog -*- */


:- module(sb_writer_dot,[
                         write_dot/1,
                         write_dot2/1
                        ]).
:- use_module(bio(sb_db)).
:- use_module(bio(graphviz)).

io:write_all(sb_dot,F):-
        write_dot(F).
io:write_all(sb_dot2,F):-
        write_dot2(F).

write_dot(F):-
        graphterm(G),
        graph_to_file(G,dot,F).
write_dot2(F):-
        graphterm2(G),
        graph_to_file(G,dot,F).

graphterm(graph(foo,SubTerms)):-
        Style=[prop(style=filled)],
        findall(node(ID,[label=N,fillcolor=red,shape=diamond]),
                reaction(ID,N),
                ReactionNodes),
        findall(node(ID,[label=Label]),
                (species(ID,N,Comp),sformat(Label,'~w[~w]',[N,Comp])),
                SpeciesNodes),
        findall(edge(RID,SID,[label=reactant,weight=100]),
                reaction_reactant(RID,SID),
                Edges1),
        findall(edge(RID,SID,[label=modifier]),
                reaction_modifier(RID,SID),
                Edges2),
        findall(edge(RID,SID,[label=product,weight=100]),
                reaction_product(RID,SID),
                Edges3),
        flatten([Style,ReactionNodes,SpeciesNodes,Edges1,Edges2,Edges3],
                SubTerms).
graphterm2(graph(foo,SubTerms)):-
        Style=[prop(style=filled)],
        findall(node(ID,[label=N,fillcolor=red,shape=diamond]),
                reaction(ID,N),
                ReactionNodes),
        findall(edge(ID1,ID2,[label=EdgeLabel]),
                (reaction_link(ID1,ID2,Via),species(Via,EdgeLabel,_)),
                Edges1),
        flatten([Style,ReactionNodes,Edges1],
                SubTerms).
        
        
/** <module>   
  @author Chris Mungall
  @version  $Revision: 1.1 $
  @date  $Date: 2006/03/18 04:00:47 $
  @license LGPL


  bridging layer from chaos-xml to native sb model

  you should not need this module directly - handled by module io
  
  */