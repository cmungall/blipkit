:- use_module(bio(metadata_db)).
:- use_module(bio(phylo_db)).
:- use_module(bio(ontol_db)).
:- use_module(bio(seqfeature_db)).
:- use_module(bio(rdb_util)).
:- use_module(bio(seqfeature_sqlmap_go)).
:- use_module(bio(ontol_sqlmap_go)).
:- use_module(bio(curation_db)).
:- use_module(bio(bioprolog_util),[solutions/3]).
:- use_module(bio(tabling)).
:- use_module(bio(simmatrix)).

% by default we get both Seq->Gene associations and Gene->Class annotations from GO.
% remove these 2 lines if the goal is to get them from a prolog db via a file.
:- sqlbind(seqfeature_db:all,_).
:- sqlbind(curation_db:curation_statement/4,_).

:- table_pred(ontol_db:subclassT/2).


% conventional to represent species as Q9VE32_DROME
% this splits such that treelabel_xref('Q9VE32_DROME','Q9VE32','DROME') is true
treelabel_xref_spcode(L,X,Sp) :-
        concat_atom([X,Sp],'_',L).

leafnode_seqid(N,X) :-
        phylonode(N),
        entity_label(N,L),
        treelabel_xref_spcode(L,X,_).
leafnode_seqid(N,X) :-
        phylonodeprop(N,'G',X). % matches RFAM-style trees (e.g. ENSEMBL IDs)

leafnode_feature(N,F) :-
        leafnode_seqid(N,X),
        debug(phylo,'seqid: ~w',[X]),
        feature_dbxref(F,X).
leafnode_feature(N,F) :-
        leafnode_seqid(N,X),
        debug(phylo,'seqid: ~w',[X]),
        feature_dbxref(F,_,X).

leafnode_feature_species(N,F,S) :-
        leafnode_feature(N,F),
        feature_organism(F,S).

leafnode_feature_taxclass(N,F,C) :-
        leafnode_feature_species(N,F,S),
        atom_concat('NCBITaxon:',S,C).

leafnode_annotation(N,F,C) :-
        leafnode_feature(N,F),
        debug(phylo,'feature: ~w',[F]),
        curation_statement(_,F,has_role,C). % queries GO db

leafnode_annotationT(N,F,CT) :-
        leafnode_annotation(N,F,C),
        parentRT(C,CT).

anode_descendent_function_distance(AncNode,LeafNode,LeafGene,Dist,C) :-
        phylonode_ancestor_distance(LeafNode,AncNode,Dist),
        leafnode_annotation(LeafNode,LeafGene,C).


make_ix :-
        generate_term_indexes(F,A,leafnode_annotationT(F,_,A)).

pair_relationship_sim(X,Y,Rel,DX/DY,Sim) :-
        simmatrix:feature_ix(X,_),
        simmatrix:feature_ix(Y,_),
        X@<Y,
        homologous_pair_relation(X,Y,Rel),
        phylo_rca([X,Y],A),
        phylonode_ancestor_distance(X,A,DX),
        phylonode_ancestor_distance(X,A,DY),
        feature_pair_simj(X,Y,Sim).

homrel_avgsim(Rel,AvgSim) :-
        (   Rel=orthology;Rel=paralogy),
        findall(Sim,pair_relationship_sim(_X,_Y,Rel,_,Sim),Sims),
        sumlist(Sims,TotalSim),
        length(Sims,NumSims),
        AvgSim is TotalSim/NumSims.

% Stuff below no longer needed..




% the taxonomy ontology may be large. We can make a slim from all
% LCAs of any extant gene-bearing nodes
taxonomy_lca(LCA) :-
        setof(C,N^F^leafnode_feature_taxclass(N,F,C),Cs),
        member(X,Cs),
        member(Y,Cs),
        X@=<Y,
        class_pair_subclass_lca(X,Y,LCA).

:- table_pred(taxonomy_lca/1).


taxonomy_lca_root(Root) :-
        taxonomy_lca(Root),
        \+ ((subclassT(Root,X),
             taxonomy_lca(X))),
        !.

taxonomy_parent(X,Y):-
        taxonomy_lca(X),
        taxonomy_parent_ext(X,Y).

taxonomy_parent_ext(X,Y):-
        subclass(X,Y),
        taxonomy_lca(Y).

taxonomy_parent_ext(X,Y):-
        subclass(X,Z),
        \+ taxonomy_lca(Z),
        taxonomy_parent_ext(Z,Y).

:- table_pred(taxonomy_parent/2).


preorder_taxonomy(L) :-
        taxonomy_lca_root(N),
        preorder_taxonomy([N],1,_,L).
preorder_taxonomy([],Ix,Ix,[]).
preorder_taxonomy([N|Ns],IxL,IxR,[N=IxL|L]) :-
        solutions(C,taxonomy_parent(C,N),Cs),
        IxL2 is IxL+1,
        preorder_taxonomy(Cs,IxL2,IxZ,L1),
        preorder_taxonomy(Ns,IxZ,IxR,L2),
        append(L1,L2,L).       % yes, difference lists may be faster..


        
/*

  TODO: translate to binary tree

infer_duplications :-
        preorder_taxonomy(OL),
        solutions(m(G,SNum),
                  (   leafnode_feature_taxclass(_,G,S),
                      member(S=SNum,OL)),
                  M),
        foo.

        


%% anode_lspecies(?G,?S)
% true if S is a species in which occur the extant genes descendant from
% ancestral node G
% all solutions equivalent to function γ(g) in Zmasek et al
anode_lspecies(G,S) :-
        phylonode_parentRT(Leaf,G),
        leafnode_feature_taxclass(Leaf,_,SA),
        subclassRT(S,SA).

anode_lspecies_lca(G,S) :-
        anode_lspecies(G,S1),
        foo.


% σ(s) in Zmasek et al
desc(S,DS) :-
        subclassRT(DS,S).

% ancestral species S that we infer harbored G
% M(g) in Zmasek
% smallest node in set of taxa S satisfying
% γ(g) ⊆ σ(M(f))
min_gene_taxon(G,S) :-
        anode_lspecies(G,S),  % γ(g)
        anode_lspecies(G,S),  % γ(g) ⊆ M(f)
        foo.

min_gene_taxon(G,S) :-
        anode_lspecies(G,S),  % γ(g)
        subclassRT(S,SA),
        %\+ ((anode_lspecies(G,S2),
             
        anode_lspecies(G,S),  % γ(g) ⊆ M(f)
             foo.
        
*/

/** <module> 

  ---+ Synopsis

==
:- use_module(bio(phylo_queries)).

% 
demo:-
  nl.
  

==

---+ Details

Bridges between

* Phylogenetic trees obtained from a NHX file using <UniProt>_<SpeciesCode> style IDs
* A GO Database containing annotations for the leaf nodes


==
blip  -u phylo_queries  -i test_data/gpcr.nhx -r godb/go -goal "load_bioresource(go)" findall "phylonode_annotation(N,F,C)" -label
==

---+ Additional Information

This module is part of blip. For more details, see http://www.blipkit.org

@author  Chris Mungall
@version $Revision$
@see     README
@license License


*/
