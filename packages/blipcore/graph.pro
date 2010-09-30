/* -*- Mode: Prolog -*- */

:- module(graph,
          [
           reflexive_closure/3,
           closure/3,
           closure/2,
           closure_dist/4,
           closure_path/4,
           closure_to_edgelist/3,
           closure_contains_cycle/1,
           closure_contains_cycle/2,
           closure_to_edgelist_delta/4,
           closure_to_edgelist_delta/5,
           siblings/3,
           edgelist_to_trees/2,
           dag_path_to_root/4,
           is_node_root/2,
           is_node_leaf/2,
           inverse/3,
           inverse/4,
           spanning_node/3,
           minimal_spanning_node/3
          ]).
:- module_transparent
        reflexive_closure/3,
        closure/3,
        closure/2,
        closure_dist/4,
        closure_path/4,
        closure_path/5,
        closure_to_edgelist/3,
        closure_to_edgelist1/4,
        closure_contains_cycle/1,
        closure_contains_cycle/2,
        closure_contains_cycle/3,
        closure_to_edgelist_delta/4,
        closure_to_edgelist_delta/5,
        siblings/3,
        dag_path_to_root/4,
        is_node_root/2,
        is_node_leaf/2,
        inverse/3,
        inverse/4,
        spanning_node/3,
        minimal_spanning_node/3.

:- use_module(bio(bioprolog_util)).
:- use_module(bio(mode)).


reflexive(_R,X,X).

%% reflexive_closure(+R,?X,?Y)
%  The reflexive closure of a relation includes the reflexive relation
%  
reflexive_closure(_R,X,X).
reflexive_closure(R,X,Y) :- closure(R,X,Y).

%% closure(+Relation,?Subj,?Obj)
%  @param Relation
%  the name of a relation, or a goal
%  @param Subj
%  subject of relation
%  @param Obj
%  object of relation
%
%   higher order predicate for transitive closure of a relation
closure(R,X,Y) :- closure(R,[],X,Y).

% closure(+Relation,+Path,?Subj,?Obj)
closure(R,_,X,Y) :- call(R,X,Y).
closure(R,P,X,Y) :- call(R,X,Z),\+member(Z,P),closure(R,[Z|P],Z,Y).

% EXPERIMENTAL
% currying: too difficult?
% eg closure(isa(human),Y) => isa(human,Z),
closure(R,Y) :- call(R,Y).
closure(R,Y) :- call(R,Z),switch_lastarg(R,Z,R2),closure(R2,Y).

% turns P(A1,A2,...,An-1,An) to P(A1,A2,...,An-1,X)
switch_lastarg(Mod:R,X,Mod:R2):-
        !,
        switch_lastarg(R,X,R2).
switch_lastarg(R,X,R2):-
        R =.. Args,
        reverse(Args,[_|RevArgsT]),
        reverse([X|RevArgsT],Args2),
        R2 =.. Args2.

%% closure_to_edgelist(+Relation,+NodeIDorIDs,?Edges)
%  @param Relation
%  name of a relation
%  @param NodeIDorIDs
%  Source(s). An ID or list of IDs. The ID should be linked from a source or target in edgelist
%  @param Edges
%  [edge(X,Y,Relation),....]
%   given a transitive relation predicate and a subject argument,
%  find the closure as an edgelist
%
%  cycle-safe
%  
:- mode closure_to_edgelist(+,+,?) is det.
closure_to_edgelist(R,NodeOrNodeList,Edges):-
        (   is_list(NodeOrNodeList)
        ->  NodeList=NodeOrNodeList
        ;   NodeList=[NodeOrNodeList]),
        list_to_set(NodeList,NodeSet),
        closure_to_edgelist1(R,NodeSet,Edges1,[]),
        list_to_set(Edges1,Edges).

% closure_to_edgelist1(+Relation,+NodeSet,?Edges,+InNodes)
:- mode closure_to_edgelist1(+,+,?,+) is det.
closure_to_edgelist1(R,[X|Xs],Edges,InNodes):-
        debug(graph,'Extending: ~w',[[X|Xs]]),
        (   setof(R-Y,call(R,X,Y),RYs) % extend by 1 to all neighbours (including those already reached)
        ->  solutions(Y,R^(member(R-Y,RYs),\+member(Y,InNodes)),Ys), % exclude visisted
            merge_set(Xs,Ys,Zs), % extend - add to list
            setof(edge(X,Y,R),member(R-Y,RYs),Edges1),
            closure_to_edgelist1(R,Zs,Edges2,[X|InNodes]), % extend by one more
            append(Edges1,Edges2,Edges) % merge_set??? difference list would be more efficient
        ;   closure_to_edgelist1(R,Xs,Edges,[X|InNodes])). % process rest of nodes
closure_to_edgelist1(_,[],[],_).

%% closure_contains_cycle(+Rel)
%   true if the closure of Rel commencing from NodeIDorIDs contains a cycle
%  
closure_contains_cycle(R):-
        debug(graph,'checking for cycles over ~w',[R]),
        setof(X,Y^call(R,X,Y),Xs),
        debug(graph,'  seed ~w',[Xs]),
        closure_contains_cycle(R,Xs).
%% closure_contains_cycle(+Rel,+NodeIDorIDs)
%   true if the closure of Rel commencing from NodeIDorIDs contains a cycle
%  
closure_contains_cycle(R,X):-
        (is_list(X)-> L=X ; L=[X]),
        closure_contains_cycle(R,L,[]),
        !.
closure_contains_cycle(_R,[X|_],InNodes):-
        member(X,InNodes),
        !.
closure_contains_cycle(R,[X|Xs],InNodes):-
        call(R,X,Y),
        closure_contains_cycle(R,[Y|Xs],[X|InNodes]).
closure_contains_cycle(R,[X|Xs],InNodes):-
        closure_contains_cycle(R,Xs,[X|InNodes]).
        
%% closure_to_edgelist_delta(+Rel,+NodeIDorIDs,?Edges,+Delta,+IsReverse)
closure_to_edgelist_delta(R,X,Edges,D,0):-
        !,
        closure_to_edgelist_delta(R,X,Edges,D).
closure_to_edgelist_delta(R,X,Edges,D,_):- % invert direction
        closure_to_edgelist_delta(R,X,Edges1,D),
        invert_edgelist(Edges1,Edges).

%% closure_to_edgelist_delta(+Rel,+NodeIDorIDs,Edges,Delta)
closure_to_edgelist_delta(R,X,Edges,down(Down)):-
        (is_list(X)-> L=X ; L=[X]),
        closure_to_edgelist(R,L,Edges1),
        debug(graph,'Extending edges dist: ~w',[Down]),
        extend_edgelist(R,L,Edges2,Down),
        append(Edges1,Edges2,Edges).
closure_to_edgelist_delta(R,X,Edges,up(Up)):-
        (is_list(X)-> L=X ; L=[X]),
        extend_edgelist(R,L,Edges,Up).

siblings(R,X,Sibs):-
        solutions(Sib,(call(R,X,Y),call(R,Sib,Y)),Sibs).

invert_edgelist([],[]).
invert_edgelist([E|EL],[E2|EL2]):-
        invert_edge(E,E2),
        invert_edgelist(EL,EL2).
invert_edge(edge(X,Y,R),edge(Y,X,R)).

%% extend_edgelist(+R,+IDs,?Edges,+Down)
:- module_transparent extend_edgelist/4.
extend_edgelist(_,_,[],0):- !.
extend_edgelist(_,[],[],_):- !.
extend_edgelist(R,Xs,Edges,Down):-
        solutions(edge(Y,X,R),(member(X,Xs),call(R,Y,X)),Edges1), % 1 down
        (   Edges1=[]
        ->  Edges=[]
        ;   setof(Y,X^R^member(edge(Y,X,R),Edges1),Ys), % extract IDs
            DownMinus is Down-1,
            extend_edgelist(R,Ys,Edges2,DownMinus), % 1-deeper
            append(Edges1,Edges2,Edges)). % merge_set???

%% edgelist_to_trees(+Edges,?Nodes)
%  given a list of edges between nodes in the graph, return a denormalized tree rooted
%  at the sources
% @param Edges [edge(A,B,R),...]
% @param Tree node(R,ID,[Tree1, Tree2, ...])
% TODO: currently two edges between the same nodes (eg inferred by
% diff means) get counted twice; just want one child in tree
edgelist_to_trees(Edges,Nodes):-
        solutions(Root,dag_root(Edges,Root),Roots),
        debug(graph,'roots=~w',[Roots]),
        findall(Node,
                (member(Root,Roots),edgelist_to_tree(root,Root,Edges,Node,[])),
                Nodes).

%% edgelist_to_tree(+R,+FromID,+Edges,?Tree,+CheckedNodes) 
edgelist_to_tree(R,ID,Edges,node(R,ID,Nodes),CheckedNodes):-
        debug(graph,'  e2t=~w',[ID]),
        solutions(CID,member(edge(CID,ID,_),Edges),CIDs), % all nodes with parent=FromID
        findall(Node,
                (   member(CID,CIDs),
                    \+ member(CID,CheckedNodes),
                    % may be multiple Rs - just choose first one, arbitrary
                    % this is 99% OK, as >1 R is only for inferred
                    % via multiple explanations
                    % ideally we want most parsimonious explanations
                    % in fact really we want to separate R from explanation
                    memberchk(edge(CID,ID,R1),Edges), % !!!!
                    edgelist_to_tree(R1,CID,Edges,Node,[CID|CheckedNodes])),                    
                Nodes).

:- module_transparent dag_root/2.
dag_root(Edges,Root):-
        member(edge(_,Root,_),Edges),
        \+member(edge(Root,_,_),Edges).

dag_path_to_root(Edges,ID,RID,EL):-
        E=edge(ID,PID,_),
        (   member(E,Edges)
        ->  EL=[E|EL1],
            dag_path_to_root(Edges,PID,RID,EL1)
        ;   EL=[],
            RID=ID).

%% is_node_root/2
%  @mode is_node_root(?R,+X)  nondet
%  @mode is_node_root(+R,+X)  semidet
%
%  
%  succeeds if X is a root node under relation R (i.e. there is no Y such that Y R X is true)
%  
%  
is_node_root(R,X):-
        (   nonvar(X)
        ->  not(call(R,X,_))
        ;   throw(error('arg unbound in graph_root'))).

%% is_node_leaf/2
%  @mode is_node_leaf(?R,+X)  nondet
%  @mode is_node_leaf(+R,+X)  semidet
%
%  
%  succeeds if X is a root node under relation R (i.e. there is no Y such that X R Y is true)
%  
%  
is_node_leaf(R,X):-
        (   nonvar(X)
        ->  not(call(R,_,X))
        ;   throw(error('arg unbound in graph_root'))).

%% inverse(+Relation,?Subj,?Obj)
%  @param Relation
%  the name of a relation, or a goal
%  @param Subj
%  subject of relation
%  @param Obj
%  object of relation
%
%   higher order predicate for transitive inverse of a relation
:- module_transparent inverse/3.
inverse(R,X,Y) :- call(R,Y,X).

%% inverse(+Relation,?Subj,?Obj,?Label)
%  @param Relation
%  the name of a relation, or a goal
%  @param Subj
%  subject of relation
%  @param Obj
%  object of relation
%  @param Label
%  additional data attached to the relation
%
%  As inverse/3 but also allows for typing of the relation
%predicate using an additional Label term
:- module_transparent inverse/4.
inverse(R,X,Y,Label) :- call(R,Y,X,Label).

%% closure_path(+Relation,?Path,?Subj,?Obj)
%  @param Relation
%  the name of a relation, or a goal
%  @param Subj
%  subject of relation
%  @param Obj
%  object of relation
%  @param Path
%  list of nodes
%  
%  computes the closure and the path between Subj and Obj
%
%  cycle-safe
%  
closure_path(R,P,S,O):-  closure_path(R,P,[],S,O).

% closure_path(+Relation,?Path,+PathIn,?Subj,?Obj)
closure_path(_R,[X],_,X,X).
closure_path(R,[X|P],Pin,X,Y) :-
        call(R,X,Z),
        \+ member(Z,Pin),
        closure_path(R,P,[Z|Pin],Z,Y).

%% closure_dist(+Relation,?Dist,?Subj,?Obj)
%  @param Relation
%  the name of a relation, or a goal
%  @param Dist
%  integer distance
%  @param Subj
%  subject of relation
%  @param Obj
%  object of relation
%  
%  computes the closure and the distance between Subj and Obj
%
%  cycle-safe
%  
closure_dist(R,D,X,Y) :- closure_path(R,P,X,Y),length(P,D1),D is D1-1.

%% spanning_node(+RelPredName,+Nodes,?SpanningNode)
%  @param RelPredName
%  the name of a TRANSITIVE relation, or a goal
%  @param Nodes
%  list of node IDs - unify with first argument in RelPredName
%  
%
%  finds a nodes SpanningNode in a graph defined by Rel such
%  that SpanningNode 'covers' all nodes in Nodes
%  (i.e. SpanningNode is a common ancestor of all nodes in Nodes)
%  
%  note: this will -not- compute the transitive closure for you
%
%  ==
%  % example data predicates
%  subclass(a,d).
%  subclass(b,e).
%  subclass(c,f).
%  subclass(d,e).
%  subclass(e,g).
%  subclass(f,g).
%  subclassT(X,Y):- closure(subclass,X,Y).
%
%  % this illustrates spanning_node/3
%  demo:-
%    ensure_loaded(bio(graph)),
%    setof(X,spanning_node(subclassT,[a,b,c],X),Xs),
%    format('spanning nodes: ~w~n',[Xs]).
%  ==
spanning_node(Rel,Nodes,SpanningNode):-
        % get pairs of Node-SNode [SNode is extent of Rel]
        findall(Node-X,
                (   member(Node,Nodes),
                    call(Rel,Node,X)),
                NodeSNodePairs),
        % find all potential spanning nodes
        setof(SNode,Node^member(Node-SNode,NodeSNodePairs),SNodes),
        % get SNodes which are in every node's extent
        member(SpanningNode,SNodes),
        forall(member(Node,Nodes),
               member(Node-SpanningNode,NodeSNodePairs)).
               
%% minimal_spanning_node(+Rel,+Nodes,?MSNode)
%
%  as spanning_node/3, but only computes the minimal set - i.e. the ones that do not subsume other members of the spanning set
%
%  if Rel defines a rooted tree rather than a DAG, then MSNode should
%  have exactly one element for any list of Nodes with 1 or more
%  elements
%  
%  
minimal_spanning_node(Rel,Nodes,MSNode):-
        setof(SNode,
              spanning_node(Rel,Nodes,SNode),
              SNodes),
        member(MSNode,SNodes),
        % MSNode is never above another SNode
        not((   member(SNode,SNodes),
                SNode\=MSNode,  % Rel may be reflexive
                call(Rel,SNode,MSNode))).
                
% -------------------- TESTS --------------------
% regression tests to ensure behaviour of module is correct;
% lines below here are not required for module functionality

unittest:edge_w_cycle(a,b).
unittest:edge_w_cycle(b,c).
unittest:edge_w_cycle(c,d).
unittest:edge_w_cycle(c,e).
unittest:edge_w_cycle(e,f).
unittest:edge_w_cycle(d,a).

unittest(load(R)=
      load_bioresource(R)/[]).

unittest(test(hi_order,
            [_=load(go)],
            (   ensure_loaded(bio(ontol_db)),
                class(ID,'cysteine metabolism'),
                setof(SubClassID,
                      closure(inverse(ontol_db:subclass),ID,SubClassID),
                      IDs),
                writeln(IDs),
                forall(member(XID,IDs),(class(XID,XN),writeln(XN))),
                length(IDs,NumIDs)),
            NumIDs>0)).

unittest(test(curry,
            [_=load(go)],
            (   ensure_loaded(bio(ontol_db)),
                class(ID,'cysteine metabolism'),
                Curry=ontol_db:subclass(ID),
                setof(SuperClassID,
                      (   closure(Curry,SuperClassID),
                          writeln(sc=SuperClassID)),
                      IDs),
                length(IDs,NumIDs)),
            NumIDs>0)).

unittest(test(curry2,
            [_=load(go)],
            (   ensure_loaded(bio(ontol_db)),
                class(ID,'cysteine metabolism'),
                Curry=ontol_db:subclass(ID),
                setof(SubClassID,
                      (   closure(inverse(Curry),SubClassID),
                          writeln(sc=SubClassID)),
                      IDs),
                length(IDs,NumIDs)),
            NumIDs>0)).

unittest(test(cycle_edgelist,
            [],
            (   closure_to_edgelist(unittest:edge_w_cycle,a,Edges),
                length(Edges,NumEdges),
                writeln(Edges)),
            (   NumEdges=6,
                closure_contains_cycle(unittest:edge_w_cycle,a)))).

unittest(test(cycle_closure_path,
            [],
            (   closure_path(unittest:edge_w_cycle,Path,c,a),
                writeln(Path)),
            (   member(d,Path),
                closure_contains_cycle(unittest:edge_w_cycle)))).

/** <module> graph closure

  ---+ Synopsis

  ==
  % This example illustrates the transitive closure of a relation
  :- use_module(bio(graph)).
  :- use_module(bio(ontol_db)).
  :- use_module(bio(io)).

  % demo: find all superclasses of cysteine metabolism
  demo:-
    load_bioresource(go),
    class(ID,'cysteine metabolism'),
    setof(SuperClassID,closure(subclass,ID,SuperClassID),SuperClassIDs),
    forall(member(SuperClassID,SuperClassIDs),
           (   class(SuperClassID,N),
               format('subclass: ~w ~w~n',[SuperClassID,N]))).
    
  ==
  
  ---+ Description

  This module contains predicates for operations on graphs. It does
not assume any particular db module - it can be used with predicates
from ontol_db (eg subclass/2), taxon_db (eg taxparent/2),
phylo_db (eg phylonode_parent/2), sb_db (eg
reaction_link/2), seqfeature_db (eg feature_relationship/3)
or your own data definitions

  ---++ Higher-order predicates

  The predicates defined here can be applied to any relation. This is
achieved using higher order programming: predicate (relation) names
can are passed as arguments

  note that this is a little slower than using the existing less
generic predicates; eg subclassT/2, parentT/3

  the advantage is that the generic code is more flexible; see below

  ---+++ Example
  
  ==
  % this illustrates the user of higher-order programming -
  % combining relations
  % here we combine graph closure over 'subclass' and inverse
  % to get superclasses
  :- use_module(bio(graph)).
  :- use_module(bio(ontol_db)).
  :- use_module(bio(io)).

  % demo: find all subclasses of cysteine metabolism
  demo:-
    load_bioresource(go),
    class(ID,'cysteine metabolism'),
    setof(SubClassID,closure(inverse(subclass),ID,SubClassID),SubClassIDs),
    forall(member(SubClassID,SubClassIDs),
           (   class(SubClassID,N),
               format('subclass: ~w ~w~n',[SubClassID,N]))).
  ==

  ---++ Notes

  The predicates in this module are declared transparent - they
inherit the context from the parent goal. This means that if you
import a predicate =|isa/2|= into your current module you can call
=|closure(isa,ID,PID)|= rather than =|closure(mymod:isa,ID,PID)|=
  
  ---++ TODO

  
    * Currying
  
  
  */
