/* -*- Mode: Prolog -*- */

:- module(ontol_writer_grapher,[
                            edges_to_grapher/3
                           ]).
:- use_module(bio(ontol_writer_text),[rcode/3,rcode_info/2,class_label_by_xp/2]).
:- use_module(bio(metadata_db)).
:- use_module(bio(ontol_db)).
:- use_module(bio(graphviz)).
:- use_module(bio(bioprolog_util)).

:- dynamic user:graphviz_ontol_param/2.
:- multifile user:graphviz_ontol_param/2.
:- multifile user:graphviz_ontol_param/3.
% todo - move to conf file

user:graphviz_ontol_param(Template,Param,_):- 
        user:graphviz_ontol_param(Template,Param).

% see http://www.graphviz.org/doc/info/attrs.html
%user:graphviz_ontol_param(graph,prop(style=filled)).
user:graphviz_ontol_param(node(ID),label=N):- entity_label(ID,N).
user:graphviz_ontol_param(node(Node),label=Label):-
        is_anonymous(Node),
        class_label_by_xp(Node,Label).
%user:graphviz_ontol_param(node(_),fillcolor=red).
user:graphviz_ontol_param(node(_),shape=box).
user:graphviz_ontol_param(node(_),fontname='/System/Library/Fonts/Courier.dfont'). % OS X only!!
user:graphviz_ontol_param(node(_),fontsize=8).
%user:graphviz_ontol_param(edge(_,_,_,_),weight=100).
user:graphviz_ontol_param(edge(_,_,is_a,_),arrowhead=empty).
user:graphviz_ontol_param(edge(_,_,is_a,_),color=green).
user:graphviz_ontol_param(edge(_,_,po,_),color=blue).
user:graphviz_ontol_param(edge(_,_,has_part,_),arrowhead=ediamond).
user:graphviz_ontol_param(edge(_,_,has_part,_),color=blue).
user:graphviz_ontol_param(edge(_,_,df,_),arrowhead=open).
user:graphviz_ontol_param(edge(_,_,df,_),color=red).
user:graphviz_ontol_param(edge(_,_,encodes,_),color=red).
user:graphviz_ontol_param(edge(_,_,encodes,_),arrowtail=invtriangle).
user:graphviz_ontol_param(edge(_,_,'OBO_REL:homologous_to',_),color=grey).
user:graphviz_ontol_param(edge(_,_,_,_),fontname='/System/Library/Fonts/Times.dfont'). % OS X only!!
user:graphviz_ontol_param(edge(_,_,R,Via),label=ELab):-
        rcode_info(Via,Info),
        (   entity_label(R,RLabel) -> true ; RLabel=R),
        sformat(ELab,'~w ~w',[RLabel,Info]).

% TOOD: this would be more elegant as a DCG
edges_to_grapher(Edges,[AllTerms],Opts):-
        ensure_loaded(bio(graphviz)),
        solutions(ID,(   member(edge(_,ID,_),Edges)
                     ;   member(edge(ID,_,_),Edges)),IDs),
        findall(Term,
                (   member(ID,IDs),
                    graph_label(ID,Label),
                    %findall(Param,user:graphviz_ontol_param(node(ID),Param,Edges),NodeParams),
                    NodeParams=[box(16,16)],
                    %NodeParams=[label:=Label],
                    Term=..[node,Label|NodeParams]),
                NodeTermsL),
        findall(Arc,
                (   member(edge(ID,PID,RPred),Edges),
                    rcode(RPred,Code,Via),
                    graph_label(ID,Label),
                    graph_label(PID,PLabel),
                    Arc=..[arc,PLabel,Label,label:=Code,arrows:=first]),
                Arcs),
        flatten([NodeTermsL,Arcs],AllTerms),
        !.

graph_label(ID,Label):- entity_label(ID,Label).

        

/** <module>   
  @author Chris Mungall
  @version  $Revision: 1.2 $
  @date  $Date: 2006/03/18 04:00:47 $
  @license LGPL

  */
