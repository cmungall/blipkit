/* -*- Mode: Prolog -*- */

:- module(ontol_writer_dot,[
                            edges_to_dot/3,
                            edges_to_display/2,
                            write_edges_to_image/2,
                            write_edges_via_dot/3
                           ]).
:- use_module(bio(ontol_writer_text),[rcode/3,rcode_info/2,class_label_by_xp/2]).
:- use_module(bio(metadata_db)).
:- use_module(bio(ontol_db)).
:- use_module(bio(dotwriter)).
:- use_module(bio(graphviz)).
:- use_module(bio(bioprolog_util)).


:- dynamic user:graphviz_color_node/2.

:- multifile user:image_display_exec/1.
:- dynamic user:image_display_exec/1.

:- multifile user:graphviz_ontol_param/2.
%:- dynamic user:graphviz_ontol_param/2.
:- multifile user:graphviz_ontol_param/3.
% todo - move to conf file

node_shape(ID,box):- class(ID),!.
node_shape(ID,box):- property(ID),!.
node_shape(_,oval).

user:graphviz_ontol_param(Template,Param,_):- 
        user:graphviz_ontol_param(Template,Param).
user:graphviz_ontol_param(Template,label=X) :-
	user:graphviz_ontol_param(Template,override(label=X)),
	!.
user:graphviz_ontol_param(edge(_,_,R,Via),label=ELab):-
        rcode_info(Via,Info),
        (   entity_label(R,RLabel) -> true ; RLabel=R),
        sformat(ELab,'~w ~w',[RLabel,Info]).
user:graphviz_ontol_param(edge(_,_,card(R,Card,Card),_),label=ELab):-
        sformat(ELab,'~w [~w]',[R,Card]).
user:graphviz_ontol_param(edge(_,_,card(R,MinCard,MaxCard),_),label=ELab):-
        sformat(ELab,'~w [~w-~w]',[R,MinCard,MaxCard]).
user:graphviz_ontol_param(edge(A,B,R,_),label=ELab):-
        range_cardinality_restriction(A,R,Card,B),
        Card\=1-inf,
        sformat(ELab,'~w [~w]',[R,Card]).
user:graphviz_ontol_param(node(ID),label=NF):-
	entity_label(ID,N),
	format_label(N,NF).
user:graphviz_ontol_param(node(Node),label=Label):-
        is_anonymous(Node),
        class_label_by_xp(Node,Label).
user:graphviz_ontol_param(node(Node),label=''):-
	\+ entity_label(Node,_),
	genus(Node,_).

user:graphviz_ontol_param(node(Node),style=filled):-
        user:graphviz_color_node(Node,_).
user:graphviz_ontol_param(node(Node),fillcolor=Col):-
        user:graphviz_color_node(Node,Col).


% see http://www.graphviz.org/doc/info/attrs.html
%user:graphviz_ontol_param(graph,prop(style=filled)).
%user:graphviz_ontol_param(node(_),fillcolor=red).
user:graphviz_ontol_param(node(ID),shape=Shape):- node_shape(ID,Shape).
user:graphviz_ontol_param(node(_),fontname=helvetica).
%user:graphviz_ontol_param(node(_),fontname='/System/Library/Fonts/Courier.dfont'). % OS X only!!
user:graphviz_ontol_param(node(_),fontsize=14).
%user:graphviz_ontol_param(edge(_,_,_,_),weight=100).
user:graphviz_ontol_param(edge(_,_,_,_),fontname='/System/Library/Fonts/Times.dfont'). % OS X only!!

dotpath(Dot):-
        (   expand_file_search_path(path_to_dot(dot),Dot)
        ->  true
        ;   Dot=dot).

edges_to_display(Edges,Opts):-
        dotpath(DotPath),
        Fmt=png,
        tmp_file(Fmt,File),
        tmp_file(dot,DotFile),
	debug(dot,'writing temp dot file: ~w',[DotFile]),
        tell(DotFile),
        edges_to_dot(Edges,Dot,Opts),
        write(Dot),
        told,
	(   member(rankdir(RankDir),Opts)
	->  true
	;   RankDir='BT'),
        sformat(Cmd,'~w -o ~w.~w -Grankdir=~w -T~w ~w',[DotPath,File,Fmt,RankDir,Fmt,DotFile]),
	debug(dot,'cmd: ~w',[Cmd]),
        shell(Cmd),
	(   user:image_display_exec(DispProg)
	->  true
	;   DispProg=open),
        sformat(DispCmd,'~w ~w.~w',[DispProg,File,Fmt]),
	debug(dot,'display cmd: ~w',[DispCmd]),
        (  shell(DispCmd)
	-> true
	;  format(user_error,'Cannot execute: ~w~nTry setting user:image_display_exec~n',[Cmd])).	   
	

%% write_edges_to_image(+Edges:list,+Opts:list)
% generates a png file
write_edges_to_image(Edges,Opts):-
        dotpath(DotPath),
        Fmt=png,
        tmp_file(dot,DotFile),
        tell(DotFile),
        edges_to_dot(Edges,Dot,Opts),
        write(Dot),
        told,
	(   member(rankdir(RankDir),Opts)
	->  true
	;   RankDir='BT'),
        sformat(Cmd,'~w -Grankdir=~w -T~w ~w',[DotPath,RankDir,Fmt,DotFile]),
        shell(Cmd).


write_edges_via_dot(Fmt,Edges,PathStem):-
        dotpath(DotPath),
        edges_to_dot(Edges,Dot,[]),
        sformat(PathToDotFile,'~w.dot',[PathStem]),
        sformat(PathToImg,'~w.~w',[PathStem,Fmt]),
        open(PathToDotFile,write,DotIO,[]),
        write(DotIO,Dot),
        close(DotIO),
        sformat(Cmd,'~w -Grankdir=BT -T~w ~w > ~w',[DotPath,Fmt,PathToDotFile,PathToImg]),
        shell(Cmd).





% TOOD: this would be more elegant as a DCG
edges_to_dot(Edges,Dot,Opts):-
        ensure_loaded(bio(graphviz)),
        solutions(ID,(   member(edge(_,ID,_),Edges)
                     ;   member(edge(ID,_,_),Edges)),IDs),
        edges_to_dot(Edges,Dot,IDs,Opts).

% slightly awkward - switch to the new dotwriter method if there are containment relations.
% TODO: switch everything to dotwriter
edges_to_dot(Edges,Dot,IDs,Opts):-
        member(containment_relations([_|_]),Opts),
        !,
        edges_to_dot_new(Edges,Dot,IDs,Opts).

edges_to_dot(Edges,Dot,IDs,Opts):-
        (   member(cluster_pred(Goal,X,Cluster),Opts),
            \+ \+ Goal
        ->  findall(X-Cluster,(member(X,IDs),Goal),XClusterPairs),
            solutions(Cluster,member(_-Cluster,XClusterPairs),Clusters),
            findall(nodeset(Cluster,Cluster,Terms),
                    (   member(Cluster,Clusters),
                        findall(Term,
                                (   member(ID-Cluster,XClusterPairs),
                                    findall(Param,user:graphviz_ontol_param(node(ID),Param,Edges),NodeParams),
                                    Term=node(ID,NodeParams)),
                                Terms)),
                    NodeTermsL)
        ;   findall(Term,
                    (   member(ID,IDs),
                        findall(Param,user:graphviz_ontol_param(node(ID),Param,Edges),NodeParams),
                        Term=node(ID,NodeParams)),
                    NodeTermsL)),
        findall(Terms,
                (   member(ID,IDs),
                    findall(edge(ID,PID,EdgeParams),
                            (   member(edge(ID,PID,RPred),Edges),
                                rcode(RPred,Code,Via),
                                findall(Param,user:graphviz_ontol_param(edge(ID,PID,Code,Via),Param,Edges),EdgeParams)),
                            Terms)),
                EdgeTermsL),
        flatten([NodeTermsL,EdgeTermsL],AllTerms),
        Style=[prop(style=filled)],
        graph_to_dot(graph(g,[Style|AllTerms]),Dot),
        !.

%% nodes_containment_edge(+IDs:list,+CMRs:list,?A,?B) is nondet
% true if there is a containment relationship between A and B, and both
% A and B are in the list of IDs
nodes_containment_edge(IDs,CMRs,A,B) :-
	member(A,IDs),
	member(B,IDs),
	A\=B,
	member(R,CMRs),
	nodes_relation_containment_edge(IDs,R,A,B).

%% nodes_relation_containment_edge(+IDs,+R,+A,+B) is semidet
%
% we must be careful with inferences and redundancy here, otherwise
% we confuse the graph.
% assume cr=[p]
% if we have a-p->b, b-p->c we don't want to nested a under c as this
% is redundant and will break graphviz.
% if we have a-i->b, b-p->c, we do want to nest both a and b under c.
% if we have a-p->b, b-i->c, we do not want to nest a under c (even though it is part of c).
nodes_relation_containment_edge(_IDs,R,A,B) :-
	parent_over(R,A,B),
	\+ ((parent_over(R,A,Z),
	     parent_over(_,Z,B))).
nodes_relation_containment_edge(_,genus,A,B) :- % todo: any intersection element?
	genus(A,B).
nodes_relation_containment_edge(_,strict_subclass,A,B) :- 
	strict_subclass(A,B).
nodes_relation_containment_edge(_,R,A,B) :-
	atom_concat('subclass-of-',B,R),
	subclass(A,B).


% this uses the new dotwriter module, and implements containment relations
edges_to_dot_new(Edges,Dot,IDs,Opts):-
        member(containment_relations(CMRs),Opts),
        debug(dot,'edges= ~w',[Edges]),
        solutions(R,member(edge(_,_,R),Edges),Rs),
        debug(dot,'all relations= ~w',[Rs]),
        debug(dot,'cmrs = ~w',[CMRs]),
        findall(A-B,
		nodes_containment_edge(IDs,CMRs,A,B),
		NestPairs),
        debug(dot,'nps= ~w',[NestPairs]),
        findall(Term,
                (   member(ID,IDs),
                    findall(Param,user:graphviz_ontol_param(node(ID),Param,Edges),NodeParams),
                    Term=node(ID,NodeParams)),
                NodeTerms),
        findall(edge(ID,PID,EdgeParams),
                (   member(edge(ID,PID,RPred),Edges),
                    rcode(RPred,Code,Via),
                    findall(Param,user:graphviz_ontol_param(edge(ID,PID,Code,Via),Param,Edges),EdgeParams)),
                EdgeTerms),
	findall(Term,
		(   setof(Param,user:graphviz_ontol_param(key(ID),Param,Edges),KeyParams),
		    Term=node(ID,KeyParams)),
		KeyTerms),
	solutions(Key-key,(user:graphviz_ontol_param(key(Key),_,_),Key\=key),KeyNestPairs),
	append(NestPairs,KeyNestPairs,AllNestPairs),
        flatten([NodeTerms,EdgeTerms,KeyTerms],AllTerms),
        G=graph(g,[],AllTerms),
        graph_nest_pairs(G,GX,AllNestPairs),
        graph_to_dot_atom(GX,Dot),
        !.

        
format_label(N,N2) :-
        concat_atom(Toks,' ',N),
        concat_atom(Toks,'\\n',N2).

        

/** <module>   
  @author Chris Mungall
  @version  $Revision: 1.2 $
  @date  $Date: 2006/03/18 04:00:47 $
  @license LGPL

  */
