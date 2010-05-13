/* -*- Mode: Prolog -*- */


:- module(blipkit_phylo,[]).

:- use_module(bio(blipkit)).
:- use_module(bio(phylo_db)).
:- use_module(bio(ontol_db)).

blipkit:example('blip -r taxnames -r taxnodes -u phylo_bridge_from_taxon phylo-lineage -n "Drosophila melanogaster"',
                'show lineage of a species from NCBI taxonomy files').
blipkit:example('blip -r taxnames -r taxnodes -u phylo_bridge_from_taxon phylo-lineage "Agama" "Leiolepsis" "Chamaeleonidae"',
                'show least common ancestor of multiple species from NCBI taxonomy files').

:- blip('phylo-lca',
        'Least common ancestor (not counting self) of a set of taxonomic nodes',
        [],
        Names,
        (   setof(Node,Name^(member(Name,Names),
                             (   phylonode(Node,Name)
                             ;   phylonode(Name,_),Node=Name)),
                  Nodes),
            forall(member(Node,Nodes),
                   (   phylonode(Node,Name),
                       format('Node: ~w ~w~n',[Node,Name]))),
            phylotree_lca(Nodes,Node),
            phylonode(Node,Name),
            format('---~nLeast common ancestor: ~w ~w~n',[Node,Name]))).

:- blip('phylo-lineage',
        'Show lineage',
        [atom([id],ID),
         atom([name,n],N)],
        Names,
        (   
            (   Names=[]
            ->  phylonode(ID,N)
            ;   member(N,Names),
                phylonode(ID,N)),
            show_lineage(ID))).

show_lineage(ID):-
        phylonode(ID,N,PID,_Len),
        !,
        format('~w ~w~n',[ID,N]),
        show_lineage(PID).
show_lineage(_).                % null parent
/** <module>   
  @author Chris Mungall
  @version  $Revision: 1.3 $
  @date  $Date: 2005/12/03 00:06:23 $
  @license LGPL

  ---+ Name
  ---++ blipkit
- simple interface to blip module functionality

  ---+ Description

  this is a submodule of blipkit for handling phylo_db*/