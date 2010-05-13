/* -*- Mode: Prolog -*- */

:- module(graphviz,
          [
           graph_to_dot/2,
           graph_to_file/3,
           display_graph/2
           ]).

%% graph_to_dot(+GraphTerm,?DotProgram)
%   converts a graph representation to a dot string
%  @param GraphTerm
%    graph(Name,Terms)
%       Term = node(ID,Props) | edge(ID1,ID2,Props)
graph_to_dot(graph(Name,GraphTerms),Dot):-
        findall(Part,
                (member(Term,GraphTerms),graph_to_dot(Term,Part)),
                Parts),
        concat_atom(Parts,DotLines),
        sformat(Dot,
                'digraph ~w {~n~w}~n',
                [Name,DotLines]).
graph_to_dot(Type=Val,Dot):-
        sformat(Dot,'~w="~w"',[Type,Val]).
graph_to_dot(prop(Prop),Dot):-
        graph_to_dot(Prop,Inner),
        sformat(Dot,'~w;~n',[Inner]).
graph_to_dot(default(Type,Props),Dot):-
        findall(PropAsDot,
                (   member(Prop,Props),
                    graph_to_dot(Prop,PropAsDot)),
                PropAsDotL),
        concat_atom(PropAsDotL,', ',Inner),
        sformat(Dot,
                '        ~w [~w];~n',
                [Type,Inner]).
graph_to_dot(node(ID,Props),Dot):-
        escape_id(node(ID),IDe),
        findall(PropAsDot,
                (   member(Prop,Props),
                    graph_to_dot(Prop,PropAsDot)),
                PropAsDotL),
        concat_atom(PropAsDotL,', ',Inner),
        sformat(Dot,
                '       ~w [~w];~n',
                [IDe,Inner]).
graph_to_dot(nodeset(ID,Label,Nodes),Dot):-
        escape_id(node(ID),IDe),
        findall(Atom,
                (   member(Node,Nodes),
                    graph_to_dot(Node,Atom)),
                Atoms),
        concat_atom(Atoms,' ',NodeSetDot),
        sformat(Dot,
                '      subgraph  cluster_~w {~n    label="~w"~n~w~n    }~n',
                [IDe,Label,NodeSetDot]).
graph_to_dot(edge(ID1,ID2,Props),Dot):-
        escape_id(node(ID1),ID1e),
        escape_id(node(ID2),ID2e),
        findall(PropAsDot,
                (   member(Prop,Props),
                    graph_to_dot(Prop,PropAsDot)),
                PropAsDotL),
        concat_atom(PropAsDotL,', ',Inner),
        sformat(Dot,
                '        ~w -> ~w [~w];~n',
                [ID1e,ID2e,Inner]).

escape_id(ID,ID2):-
        compound(ID),
        !,
        ID=..L,
        maplist(escape_id,L,L2),
        concat_atom(L2,'__',ID2).
escape_id(ID,ID2):-
        atom_chars(ID,L),
        escape_chars(L,L2),
        atom_chars(ID2,L2).


escape_chars([],[]).
escape_chars([H|T],[H|T2]):-
        safe_char(H),
        !,
        escape_chars(T,T2).
escape_chars([_|T],['_','_'|T2]):-
        escape_chars(T,T2).

safe_char(C):- C @>= a, C @=< z,!.
safe_char(C):- C @>= 'A', C @=< 'Z',!.
safe_char(C):- C @>= '0', C @=< '9',!. % '0
safe_char('_').
        

%% graph_to_file(+GraphTerm,?Fmt,?File)
%   writes a graph to an img file using 'dot'
%
%  @param File
%   if unground, a temp file will be chosed
%  @param Fmt
%   if unground, a dot format will be chosen
graph_to_file(GraphTerm,Fmt,File):-
        (nonvar(Fmt) -> true ; Fmt=dot),
        (nonvar(File) -> true ; tmp_file(Fmt,File)),
        graph_to_dot(GraphTerm,Dot),
        (   Fmt=dot
        ->  DotFile=File
        ;   tmp_file(dot,DotFile)),
        tell(DotFile),
        write(Dot),
        told,
        (   Fmt=dot
        ->  true
        ;   concat_atom([dot,'-o',File,'-T',Fmt,DotFile],' ',Cmd),
            shell(Cmd)).

%% display_graph(+GraphTerm,+Cmd)
%   displays a graph using Cmd
display_graph(GraphTerm,DisplayCmd):-
        display_graph(GraphTerm,_,DisplayCmd).
display_graph(GraphTerm,Fmt,DisplayCmd):-
        graph_to_file(GraphTerm,Fmt,File),
        concat_atom([DisplayCmd,File],' ',FullCmd),
        shell(FullCmd).

test:-
        graph_to_dot(graph(foo,[
                                node(x,[label=x]),
                                node(y,[label=y]),
                                node(z,[label=z,fontname='Courier']),
                                edge(x,y,[label=is]),
                                edge(x,z,[label=is])
                               ]),
                     Dot),
        writeln(Dot).
/** <module> maps graph structures to dot format

  ---+ Deprecated

  Use ../graph/dotwriter.pro instead - see graph_to_dot_file/3

  ---+ Synopsis

  ==
  :- use_module(bio(graphviz)).

  % data: nodes are reactions, edges are input/outputs
  n(1, 'cyclin_cdc2k dissociation').
  n(2, 'cdc2k phosphorylation').
  n(3, 'cdc2k dephosphorylation').
  e(1,2,'cdc2k').
  e(2,3,'cdc2k-p').
  e(3,2,'cdc2k').
  
  % demo: convert prolog db to a graph diagram
  % outputs: <img alt="biochemical pathway" src="cdc.png"/>
  % <cdc.png> CDC
  demo:-
    findall(node(ID,[label=N,font='Verdana']),n(ID,N),Nodes),
    findall(edge(ID1,ID2,[label=Type,weight=1]),e(ID1,ID2,Type),Edges),
    flatten([Nodes,Edges],Subterms),
    G=graph(my_graph,Subterms),
    graph_to_file(G,png,'cdc.png'),
    writeln('Please view the file "cdc.png" in an image viewer').
  ==
  
  ---+ Description

  utility module - maps a graph data structure into a dot program. dot
files can be imported into applications like OmniGraffle, displayed
using imagemagick, or transformed into pngs etc using GraphViz
programs 'dot' and 'neatto'
  
  ---++ Data structure

  ==
  GraphTerm = graph(Name,Terms)
       Term = node(ID,Props) | edge(ID1,ID2,Props)
  ==
  
  ---+ Dependencies

  AT&T GraphViz (the 'dot' or 'neato' executable)
  
  */
