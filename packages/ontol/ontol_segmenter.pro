:- module(ontol_segmenter,
	  [
	   ontology_segment/3,
	   ontology_segment/4
	   ]).

:- use_module(ontol_db).
:- use_module(bio(graph)).
:- use_module(bio(bioprolog_util),[solutions/3]).


%% ontology_segment(+InNodes,?OutNodes,?Opts)
ontology_segment(InNodes,OutNodes,Opts):-
        ontology_segment(InNodes,_,OutNodes,Opts).
%% ontology_segment(+InNodes,?Edges,?OutNodes,?Opts)
% OutNodes = DEPRECATED
ontology_segment(InNodes,Edges,OutNodes,Opts):-
        debug(ontol,'Segmenting: ~w Opts: ~w',[InNodes,Opts]),
        (   member(relations(Rels),Opts),
            Rels\=[]
        ->  true
        ;   member(exclude_relations(ExcRels),Opts)
        ->  solutions(Rel,(parent(_,Rel,_),\+member(Rel,ExcRels)),Rels)
        ;   Rels=[]),
        relation_closure_predicate(Rels,Pred,Opts), % unify Pred with Goal for matching node to parents
        debug(ontol,'Closure pred: ~w ',[Pred]),
        (   member(maxdown(MaxDown),Opts)
        ->  true
        ;   MaxDown=0),
        (   member(siblings(1),Opts)
        ->  solutions(PNode,(member(Node,InNodes),call(Pred,Node,PNode)),InNodesPlus),
            Delta=down(1)
        ;   member(maxup(MaxUp),Opts),nonvar(MaxUp)
        ->  Delta=up(MaxUp)
        ;   InNodesPlus=InNodes,
            Delta=down(MaxDown)),
        debug(ontol,'Will use ~w for graph traversal. delta=~w',[Pred,Delta]),
        closure_to_edgelist_delta(Pred,InNodesPlus,Edges1,Delta,0), % todo IsReverse
        debug(ontol_segment,'collapsing: ~w',[Edges]),
        % todo: optimize this
        (   member(collapse_relation(CR),Opts)
        ->  collapse_edgelist_via_pred(Edges1,Edges,collapse_relation(CR)) % todo
        ;   member(collapse_predicate(CP),Opts)
        ->  collapse_edgelist_via_pred(Edges1,Edges,CP)
        ;   Edges=Edges1),
        OutNodes=[].
/*
        debug(ontol_segment,'Converting edges to trees: ~w',[Edges]),
        edgelist_to_trees(Edges,Trees),
        debug(ontol_segment,'Converted edges to trees: ~w',[Trees]),
        solutions(OutNode,
                  (   member(Tree,Trees),
                      tree_node_ids(Tree,OutNodes1),
                      member(OutNode,OutNodes1)),
                  OutNodes).
*/

collapse_edgelist_via_pred(L,L,fail):- !.
collapse_edgelist_via_pred(EdgesIn,EdgesOut,Pred):-
        findall(N,(   member(edge(N,_,_),EdgesIn)
                  ;   member(edge(_,N,_),EdgesIn)),Nodes),
        findall(N,(member(N,Nodes),user:call(Pred,N,EdgesIn)),NodesToGo),
        collapse_edgelist(EdgesIn,EdgesOut,NodesToGo,EdgesIn).


collapse_edgelist([E|EL],[E|EL2],ELC,ELO):-
        !,
        % pass through
        debug(collapse,'pass: ~w',[E]),
        collapse_edgelist(EL,EL2,ELC,ELO).
collapse_edgelist([],[],_,_):- !.

collapse_relation(R,Edge,EdgesIn):-
        Edge=edge(_,Y,R),
        member(edge(Y,_,R),EdgesIn).

user:collapse_is_a(Edge,EdgesIn):-
        Edge=edge(_,Y,relation_link(subclass)),
        member(edge(Y,_,relation_link(subclass)),EdgesIn).

relation_closure_predicate(R,inverse(P),Opts):-
        select(invert(1),Opts,OptsRemaining),
        !,
        relation_closure_predicate(R,P,OptsRemaining).
relation_closure_predicate(all,relation_link(_),_):- !.
relation_closure_predicate([],relation_link(_),_):- !. % empty=all
relation_closure_predicate([R],parent_over_nr(R,_),_):- !. % [R]->R
relation_closure_predicate(Rs,parent_over_oneof(_-Rs),_):- is_list(Rs),!.
relation_closure_predicate(R,parent_over_nr(R,_),_).

%% parent_over_oneof(+RTerm,?Node,?PNode)
% RTerm = Rel-Rels
% true if Rel is a member of Rels, and PNode can be reached from Node via Rel
parent_over_oneof(R-Rs,Node,PNode):- member(R,Rs),parent_over_nr(R,Node,PNode).

% inverts order of arguments so it can be used in a functional style
% (instances AND classes)
relation_link(R,Node,PNode):- node_link(Node,R,PNode),\+exclude_relation_in_closure(R).
%exclude_relation_in_closure(has_part).  % !!! HACK!! sometimes we want this. works fine for Cell in FMA
exclude_relation_in_closure(R):- is_cyclic(R).

% helper predicates (duped with blipkit_ontol)

% tree_node_ids(+N,?NodeSet) is semidet
% NodeSet has no dupes
% N=node(_,Node,SubNodes)
tree_node_ids(N,Nodes):-
        tree_node_id_list(N,Nodes1),
        list_to_set(Nodes1,Nodes).

% tree_node_ids(+N,?NodeList)t
% NodeList may have dupes
tree_node_id_list(node(_,Node,Nodes),[Node|Nodes]):-
        findall(SubNodes,(member(Node,Nodes),tree_node_id_list(Node,SubNodes)),SubNodesL),
        flatten(SubNodesL,Nodes).
