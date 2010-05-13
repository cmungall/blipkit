/* -*- Mode: Prolog -*- */


:- module(seqfeature_writer_dot,[
                            edges_to_dot/3,
                            write_edges_via_dot/3
                           ]).
:- use_module(bio(metadata_db)).
:- use_module(bio(seqfeature_db)).
:- use_module(bio(graphviz)).
:- use_module(bio(bioprolog_util)).

:- dynamic user:graphviz_seqfeature_param/2.
:- multifile user:graphviz_seqfeature_param/2.
:- multifile user:graphviz_seqfeature_param/3.
% todo - move to conf file

io:write_all(seqfeature_dot,F,_):-
        graphterm(G),
        graph_to_file(G,dot,F).
        
write_dot(F):-
        graphterm(G),
        graph_to_file(G,dot,F).

graphterm(graph(foo,SubTerms)):-
        Style=[prop(style=filled)],
        findall(node(ID,[label=Label,fillcolor=red,shape=box|NodeParams]),
                (   feature(ID),
                    (   entity_label(ID,N)->true ; N=ID),
                    (   feature_type(ID,T)->true ; T=''),
                    sformat(Label1,'~w (~w)',[N,T]),
                    concat_atom(Toks,'test:',Label1),
                    concat_atom(Toks,'',Label),
                    findall(Param,user:graphviz_seqfeature_param(node(ID,T),Param),NodeParams)),                    
                FNodes),
        findall(edge(X,Y,[weight=100|EdgeParams1]),
                (   
                    feature_relationship(X,Y,T),
                    findall(Param,user:graphviz_seqfeature_param(edge(ID,PID,T),Param),EdgeParams1)),
                FGEdges),
        findall(edge(X,Src,[label=Label,weight=100|EdgeParams2]),
                (   featureloc(X,Src,Beg,End,Strand),
                    sformat(Label,'~w-~w~w',[Beg,End,Strand]),
                    findall(Param,user:graphviz_seqfeature_param(edge(ID,PID,loc),Param),EdgeParams2)),
                LGEdges),
        flatten([Style,FNodes,FGEdges,LGEdges],
                SubTerms).

user:graphviz_seqfeature_param(Template,Param,_):- 
        user:graphviz_seqfeature_param(Template,Param).

% see http://www.graphviz.org/doc/info/attrs.html
%user:graphviz_seqfeature_param(graph,prop(style=filled)).
%user:graphviz_seqfeature_param(node(ID,T),label=N):- entity_label(ID,N)
%user:graphviz_seqfeature_param(node(_),fillcolor=red).
user:graphviz_seqfeature_param(node(_,_),shape=box).
user:graphviz_seqfeature_param(node(_,_),fontname='/System/Library/Fonts/Courier.dfont'). % OS X only!!
user:graphviz_seqfeature_param(node(_,_),fontsize=10).
user:graphviz_seqfeature_param(edge(_,_,loc),color=green).
user:graphviz_seqfeature_param(edge(_,_,_),fontsize=10).
user:graphviz_seqfeature_param(edge(_,_,'test:part_of'),label='p').
user:graphviz_seqfeature_param(edge(_,_,_),fontname='/System/Library/Fonts/Times.dfont'). % OS X only!!

edges_to_dot(Edges,Dot,_):-
        ensure_loaded(bio(graphviz)),
        solutions(ID,(   member(edge(_,ID,_),Edges)
                     ;   member(edge(ID,_,_),Edges)),IDs),
        findall(Terms,
                (   member(ID,IDs),
                    findall(Param,user:graphviz_seqfeature_param(node(ID),Param,Edges),NodeParams),
                    Main=node(ID,NodeParams),
                    findall(edge(ID,PID,EdgeParams),
                            (   member(edge(ID,PID,RPred),Edges),
                                rcode(RPred,Code,Via),
                                findall(Param,user:graphviz_seqfeature_param(edge(ID,PID,Code,Via),Param,Edges),EdgeParams)),
                            ETerms),
                    Terms=[Main|ETerms]),
                TermsL),
        flatten(TermsL,AllTerms),
        Style=[prop(style=filled)],
        graph_to_dot(graph(g,[Style|AllTerms]),Dot),
        !.

write_edges_via_dot(Fmt,Edges,PathStem):-
        edges_to_dot(Edges,Dot,[]),
        sformat(PathDot,'~w.dot',[PathStem]),
        sformat(PathFmt,'~w.~w',[PathStem,Fmt]),
        open(PathDot,write,DotIO,[]),
        write(DotIO,Dot),
        close(DotIO),
        sformat(Cmd,'dot -T~w ~w > ~w',[Fmt,PathDot,PathFmt]),
        shell(Cmd).
        

/** <module>   
  @author Chris Mungall
  @version  $Revision: 1.2 $
  @date  $Date: 2006/03/18 04:00:47 $
  @license LGPL

  */
