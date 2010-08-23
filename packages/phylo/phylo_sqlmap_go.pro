:- module(phylo_sqlmap_go,[]).

:- use_module(bio(sql_compiler)).
:- use_module(bio(rdb_util)).
:- use_module(bio(phylo_db),[]).
:- use_module(bio(metadata_db),[]).

:- load_schema_defs(bio('sql_schema/schema_go')).
:- [bio(ontol_sqlmap_go_util)].

:- multifile
	system:term_expansion/2.

phylo_db:phylotree(H) <-
        phylotree(_,H,_).

phylo_db:phylonode_tree(G,H) <-
        gene_product_phylotree(_,GI,HI),product0(GI,G,_),phylotree(HI,H,_).

/** <module> Maps phylo_db predicates to GO database phyloset tables

  ---+ Synopsis

  ==
  :- use_module(bio(phylo_bridge_from_gosql)).
  :- phylo_bridge_from_gosql:set_bridge_resource(go_dmel).

  demo:-
    gene(GID,GN),
    gene_transcript(GID,TID),
    transcript(TID,TN),
    feature_residues(TID,Seq),
    format('Gene:~w ~w ~w~n',[GN,TN,Seq]),
    fail.
  ==

  ---+ Description

  This allows a SQL Database using the Go Schema to masquerade as phylo
  predicates

  See <http://www.gmod.org/schema> Go

  Command line:
  
  ==
  blip-sql prolog-to-sql -u phylo_sqlmap_go "phylotree_member(G,X)" -proj X
  ==

  ==
  blip-godb prolog-to-sql -u phylo_sqlmap_go "phylotree_member(G,X)" -proj X
  ==
  
  
  @see go_database_tutorial.txt
  
  */



