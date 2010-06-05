/* -*- Mode: Prolog -*- */


:- module(phylo_db,
          [
           phylonode/1,
           phylogeny/2,
           phylonode/2,
           phylonode/4,
           phylonodeprop/3,
           phylotree/1,
           phylotree_binary/1,
           phylo_rca/2,
           homologous_pair_relation/3,
           orthologous_pair/2,
           iso_orthologous_pair/2,
           is_speciation/1,
           paralogous_pair/2,
           phylonode_tree/2,
           phylotree_monophyletic/2,
           phylonode_branchlen/2,
           phylonode_ancestor_distance/3,
           phylonode_speciations/2,
           phylonode_duplications/2,
           phylonode_confidence/3,
           phylonode_taxon/2,
           phylonode_taxon_ratio/4,
           phylonode_xref/2,
           phylonode_parent/2,
           phylonode_parentT/2,
           phylonode_parentRT/2,
           phylonode_depth/2,
           phylonode_height/2,
           phylonode_leaf/1,
           phylonode_root/1,

           phylotree_index/2
          ]).


:- use_module(library(ordsets)).

:- use_module(bio(dbmeta)).
:- use_module(bio(bioprolog_util)).
:- use_module(bio(graph)).
:- use_module(bio(mode)).
:- use_module(bio(metadata_db)).

%% phylonode(?Node)
% represents a node in a phylogenetic tree
:- extensional(phylonode/1).

%% phylogeny(?Phylogeny,?Rooted)
:- extensional(phylogeny/2).    % redundant with phylotree..?

%% phylonode(?Node,?Name)
%  combination of phylonode/1 and entity_label/2
phylonode(Node,Name):-
        entity_label(Node,Name),phylonode(Node).

%% phylonode_tree(?Node,?Tree)
:- extensional(phylonode_tree/2).

%% phylotree(?Node)
:- extensional(phylotree/1).

% TODO - partially redundant with phylogeny/2
%phylotree(Node):-
%        phylonode(Node),
%        \+ phylonode_parent(Node,_,_).


%% phylonode_parent(?Node,?PNode,?BranchLen)
% @param Node node in phylogenetic tree
% @param PNode parent of Node
% @param Len branch length between Node and PNode
:- extensional(phylonode_parent/3).

%% phylonode_parent(?Node,?ParentNode)
% @param Node node in phylogenetic tree
% @param PNode parent of Node
phylonode_parent(Node,ParentNode):-
        phylonode_parent(Node,ParentNode,_).

%% phylonode_branchlen(?Node,?BranchLen:float)
% @param Node node in phylogenetic tree
% @param BranchLen branch length between Node and its parent (see phylonode_parent/2)
phylonode_branchlen(Node,Len):-
        phylonode_parent(Node,_,Len).

%% phylonode_duplications(?Node,?Duplications:int)
:- extensional(phylonode_duplications/2).

%% phylonode_speciations(?Node,?Speciations:int)
:- extensional(phylonode_speciations/2).

%% phylonode_confidence(?Node,?Type,?Confidence:float)
% Type = bootstrap | ...
:- extensional(phylonode_confidence/3).

%% phylonode_taxon(?Node,?Taxon)
:- extensional(phylonode_taxon/2).

%% phylonode_taxon_ratio(+Node,?I,?U,?Ratio)
% | S1 ∩ S2| / | S1 ∪ S2|
% where S1 and S2 are the children of Node.
% we expect duplications to have a gene persisting in most
% persistent lineages, so the Ratio is higher
% See: Duplication consistency score im
% EnsemblCompara GeneTrees: Complete, duplication-aware phylogenetic trees in vertebrates
% Vilella et al, Genome Res 2009
phylonode_taxon_ratio(Node,I,U,Ratio) :-
        setof(C,phylonode_parent(C,Node),[X,Y]), % binary only
        setof(TX,phylonode_descendant_taxon(X,TX),TXs),
        setof(TY,phylonode_descendant_taxon(Y,TY),TYs),
        ord_intersection(TXs,TYs,TI),
        ord_union(TXs,TYs,TU),
        length(TI,I),
        length(TU,U),
        Ratio is I/U.

phylonode_descendant_taxon(N,T) :- phylonode_taxon(N,T).
phylonode_descendant_taxon(N,T) :- phylonode_parent(C,N),phylonode_descendant_taxon(C,T).



%% phylonode_xref(?Node,?Xref)
:- extensional(phylonode_xref/2).

%% phylotree_binary(?Tree)
% true if Tree is binary
phylotree_binary(Tree) :-
        \+ ((        phylonode_tree(X,Tree),
                     phylonode_parent(A,X),
                     phylonode_parent(B,X),
                     A\=B,
                     phylonode_parent(C,X),
                     A\=C,
                     B\=C)).

             

%% phylonode(Node,Name,ParentNode,BranchLen)
% DEPRECATED
phylonode(Node,Name,ParentNode,BranchLen):-
        phylonode_parent(Node,ParentNode,BranchLen)
        leftjoin entity_label(Node,Name).


%% phylonode_parentT(?Node,?PNode)
%   transitive closure of phylonode_parent/2
phylonode_parentT(Node,PNode):-  phylonode_parent(Node,PNode).
phylonode_parentT(Node,PNode):-
        phylonode_parent(Node,XNode),
        phylonode_parentT(XNode,PNode).
        
%% phylonode_parentRT(?Node,?PNode)
%   reflexive transitive closure of phylonode_parent/2
phylonode_parentRT(Node,Node).
phylonode_parentRT(Node,PNode):-  phylonode_parentT(Node,PNode).

%% phylonode_depth(?Node,?Depth:float)
%  @param Depth ∑ [b ∈ path-to-root(n)] branchlen(b)
%  sum of branchlengths to root
phylonode_depth(Node,Depth):-
        (phylonode_parent(Node,PNode)
        ->  phylonode_depth(PNode,Depth1)
        ;   Depth1=0),
        phylonode_branchlen(Node,BranchLen),
        Depth is BranchLen + Depth1.

%% phylonode_height(?Node,?Height:float)
%  @param Depth
%  max sum of branchlengths to leaf
phylonode_height(Node,Height):-
        (setof(Height1,CNode^(phylonode_parent(CNode,Node),
                           phylonode_height(CNode,Height1)),Heights)
        ->  list_max(Heights,Height1),
            phylonode_branchlen(Node,BranchLen),
            Height is BranchLen + Height1            
        ;   Height=0).

%% phylonode_ancestor_distance(+Node,?AncestorNode,?BranchLen:float)
phylonode_ancestor_distance(Node,ParentNode,BranchLen) :-
        phylonode_parent(Node,ParentNode,BranchLen).
phylonode_ancestor_distance(Node,AncestorNode,BranchLen) :-
        phylonode_parent(Node,ParentNode,BranchLen),
        phylonode_ancestor_distance(ParentNode,AncestorNode,BranchLenCum),
        BranchLen is BranchLen + BranchLenCum.

%% phylonode_leaf(?Node)
% true if Node is parent of nothing
phylonode_leaf(Node):-
        phylonode(Node),
        \+phylonode_parent(_,Node).

%% phylonode_root(?Node)
% true if Node has no parents
phylonode_root(Node):-
        phylonode(Node),
        \+phylonode_parent(Node,_).

%% phylonodeprop(?Node,?Key,?Value)
:- extensional(phylonodeprop/3).

%% phylotree_topology(?Tree,?Topology)
%
% Topology = topology(Nodes:list,Edges:set)
% Edge = edge(From,To,Length:float)
phylotree_topology(Tree,topology(Nodes,Edges)) :-
        solutions(Node,phylonode_tree(Node,Tree),Nodes),
        solutions(edge(From,To,Len),
                  (   phylonode_tree(To,Tree),
                      phylonode_parent(From,To,Len)),
                  Edges).

% TODO: check
robinson_foulds_distance(T1,T2,Dist) :-
        T1=topology(_,Edges1),
        T2=topology(_,Edges2),
        !,
        ord_intersection(Edges1,Edges2,EdgesI),
        ord_union(Edges1,Edges2,EdgesU),
        ord_subtract(EdgesU,EdgesI,EdgesD),
        length(EdgesD,Dist).

weighted_robinson_foulds_distance(T1,T2,Dist) :-
        T1=topology(_,Edges1),
        T2=topology(_,Edges2),
        !,
        ord_union(Edges1,Edges2,EdgesU),
        solutions(A-B,member(edge(A,B,_),EdgesU),UnwEdges),
        aggregate(sum(D1),A-B,(member(A-B,UnwEdges),edge_diff(A,B,Edges1,Edges2,D1)),Dist).

branch_score_distance(T1,T2,Dist) :-
        T1=topology(_,Edges1),
        T2=topology(_,Edges2),
        !,
        ord_union(Edges1,Edges2,EdgesU),
        solutions(A-B,member(edge(A,B,_),EdgesU),UnwEdges),
        aggregate(sum(D2),A-B,(member(A-B,UnwEdges),edge_diff(A,B,Edges1,Edges2,D1),D2 is D1*D1),Sum),
        Dist is sqrt(Sum).

edge_diff(A,B,Es1,Es2,D) :-
        (   member(edge(A,B,W1),Es1)
        ->  true
        ;   W1=0),
        (   member(edge(A,B,W2),Es2)
        ->  true
        ;   W2=0),
        D is abs(W1-W2).

nlist_to_edgelist(X-Y,[edge(X,Y,1)]) :- !.
nlist_to_edgelist(L,Edges) :-
        findall(Es,(member(L1,L),nlist_to_edgelist(L1,Es)),Es2),
        flatten(Es2,Edges).
nlist_to_topology(L,topology(Nodes,Edges)) :-
        nlist_to_edgelist(L,EdgeList),
        list_to_ord_set(EdgeList,Edges),
        solutions(N,(   member(edge(N,_,_),Edges)
                    ;   member(edge(_,N,_),Edges)),Nodes).

:- begin_tests(distance,[]).
test(rf) :-
        nlist_to_topology([a-b,c-d,e-f],T1),
        nlist_to_topology([a-c,d-f,b-e],T2),
        robinson_foulds_distance(T1,T2,6).
test(wrf) :-
        nlist_to_topology([a-b,c-d,e-f],T1),
        nlist_to_topology([a-c,d-f,b-e],T2),
        robinson_foulds_distance(T1,T2,6).
        
:- end_tests(distance).


%% phylo_rca(+Nodes:list,?LCA)
% @param Nodes - a list of Node identifiers
% @param LCA -  least common ancestor Node
:- mode phylo_rca(+,?) is det.
phylo_rca(Nodes,LCA):-
        setof(A,
              minimal_spanning_node(phylo_db:phylonode_parentT,Nodes,A),
              [LCA]).


% we could also do this without graph module...
%phylo_rca([Node|Nodes],LCA):-
%       phylo_rca(Node,Nodes,LCA).

% (+LCA1,+Nodes,?LCA)
%phylo_rca(LCA,[],LCA).
%phylo_rca(Node1,[Node2|Nodes],LCA):-
%        phylonodepair_lca(Node1,Node2,LCA1),
%        phylo_rca(LCA1,Nodes,LCA1).

%% phylotree_monophyletic(+Nodes:list,+OutGroup) is semidet
% true if OutGroup is not a descendant of the LCA of Nodes
%
% @see phylo_rca/2, phylonode_parentT/2
phylotree_monophyletic(Nodes,OutGroup):-
        phylo_rca(Nodes,LCA1),
        \+ (   phylonode_parentT(OutGroup,A2),
               A2=LCA1).

%% orthologous_pair(?A,?B)
% true if A and B are orthologous
% (i.e. phylo_rca/2 is speciation event)
orthologous_pair(A,B):- homologous_pair_relation(A,B,orthology).

%% paralogous_pair(?A,?B)
% true if A and B are paralogous
% (i.e. phylo_rca/2 is non-speciation/duplication event)
paralogous_pair(A,B):- homologous_pair_relation(A,B,paralogy).

%% iso_orthologous_pair(?A,?B)
% true if A and B are iso-orthologs
iso_orthologous_pair(A,B):-
        orthologous_pair(A,B),
        phylonode_taxon(B,T),
        \+ ((orthologous_pair(A,C),
             B\=C,
             phylonode_taxon(C,T))).

homologous_pair_relation(A,B,Rel):-
        %phylonode(A),
        %phylonode(B),
        phylonode_leaf(A),
        phylonode_leaf(B),
        A\=B,
        phylo_rca([A,B],X),
        (   is_speciation(X)
        ->  Rel=orthology
        ;   Rel=paralogy).

%% is_speciation(?Node)
% true if Node represents a speciation event.
% currently detected based on phylonode_speciations/2
is_speciation(X):- phylonode_speciations(X,Num),Num>0.


% EXPERIMENTAL
phylotree_index(Node,Pairs):-
        phylotree(Node),
        phylonode_index(Node,Pairs,1,_).

% TODO
phylonode_index(Node,[idx(Left,Right,Node)|Pairs],Left,Right):-
        phylonode(Node),
        debug(phylo,'indexing ~w',[Node]),
        (   phylonode_leaf(Node)
        ->  Right is Left+1
        ;   setof(CNode,phylonode_parent(CNode,Node),CNodes),
            Left2 is Left+1,
            phylonodes_index(CNodes,Pairs,Left2,Right)).
phylonodes_index([],[],Left,Left).
phylonodes_index([Node|Nodes],Pairs,Left,Right2):-
        phylonode_index(Node,Pairs1,Left,Right),
        Left2 is Right+1,
        phylonodes_index(Nodes,Pairs2,Left2,Right2),
        append(Pairs1,Pairs2,Pairs). % TODO: diff-L



% -------------------- TESTS --------------------
% regression tests to ensure behaviour of module is correct;
% lines below here are not required for module functionality

unittest(load(test_nhx)=
      load_biofile(nhx,'test.nhx')/[]).
unittest(load(test2_nh)=
      load_biofile(nh,'test2.nh')/[]).
unittest(load(bcl2)=
      load_biofile(phyloxml,'bcl_2.phylo-xml')/[]).

/*
unittest(test(load_nhx_file,
             [_=load(test_nhx)],
             (   ensure_loaded(bio(phylo_db)),
                 phylotree_index(Node,Pairs),
                 writeln(Node-Pairs)),
            true)).
*/

unittest(test(monophyletic,
             [_=load(test2_nh)],
             (   ensure_loaded(bio(phylo_db)),
                 phylonode(Node1,hADH1),
                 phylonode(Node2,hADH2),
                 phylonode(OutGroup,yADH4)),
            phylotree_monophyletic([Node1,Node2],OutGroup))).

unittest(test(non_monophyletic,
             [_=load(test2_nh)],
             (   ensure_loaded(bio(phylo_db)),
                 setof(Node,(phylo_db:mixgroup(N),phylonode(Node,N)),Nodes),
                 phylonode(OutGroup,iADHX)),
            \+ phylotree_monophyletic(Nodes,OutGroup))).

unittest(test(orthology,
             [_=load(bcl2)],
             (   ensure_loaded(bio(phylo_db)),
                 ensure_loaded(bio(metadata_db)),
%                 forall(homologous_pair_relation(A,B,R),
%                        writeln([R,A,B]))),
                 forall((iso_orthologous_pair(A,B),A@<B,entity_label(A,AN),entity_label(B,BN)),
                        writeln([AN-BN]))),
            true)).

mixgroup(hADH1).
mixgroup(yADH2).
mixgroup(yADH3).


/** <module>  phylogenetic tree data module

  ---+ Synopsis
  
  ==
  :- use_module(bio(io)).
  :- use_module(bio(phylo_db)).

  % show height of all trees in file
  demo:-
    load_biofile(nhx,'test.nhx'),
    forall(phylotree(Node),
           (   phylonode_height(Node,Height),
               format('tree ~w has height ~w~n',[Node,Height]))).
  ==
  
  ---+ Description

  Models phylogenetic trees, representing the evolution of sequences
  based on sequence alignment

  See README
  
  ---++ Importing and exporting data

  This is a data module. Facts can be imported and exported from both
prolog fact databases and other formats
  
  ---+++ Import

  The following file formats can be read in using load_biofile/2
and load_bioresource/1
  
  * nh
  * nhx
  * phyloxml  

  ---+++ Export

  The following file formats can be written using write_biofile/2
  
  * chadoxml - via phylo_bridge_to_chadoxml.pro
  
  @author Chris Mungall
  @version  $Revision: 1.12 $
  @license LGPL
  @see README
  
  */
