/* -*- Mode: Prolog -*- */


:- module(phylo_sqlmap_enscompara,
          [
          ]).

:- use_module(bio(sql_compiler)).
:- use_module(bio(rdb_util)).
:- use_module(bio(ontol_db),[]).
:- use_module(bio(metadata_db),[]).
:- use_module(bio(phylo_db),[]).

:- load_schema_defs(bio('sql_schema/schema_enscompara')).

:- multifile
	system:term_expansion/2.

dbxref_iid_id(DBXrefID,X) <-
  dbxrefd(DBXrefID,_,_,_,_,_,X). 


%metadata_db:entity_label(Node,Name) <- phylonode(Node,_,_,_,_,_,_,Name,_).
phylo_db:phylonode_branchlen(Node,Len) <- protein_tree_node(Node,_,_,_,_,Len).
phylo_db:phylonode_parent(Node,P) <- protein_tree_node(Node,P,_,_,_,_).
phylo_db:phylonode_xref(Node,X) <- protein_tree_member(Node,Member,_,_,_,_),member(Member,X,_,_,_Tax).
phylo_db:phylonode_taxon(Node,Tax) <- protein_tree_member(Node,Member,_,_,_,_),member(Member,_,_,_,Tax).

%phylo_db:phylonode_tree(Node,SubRoot) <- phylonode_parentT(Node,SubRoot), protein_tree_node(SubRoot,TopRoot,TopRoot,_,_,_).
phylo_db:phylonode_tree(Node,SubRoot) <- phylonode_parentT(Node,SubRoot), phylonode_root(SubRoot).

% see ensembl docs. parent_id=root_id
phylo_db:phylonode_root(Node) <- protein_tree_node(Node,Root,Root,_,_,_).
phylo_db:phylonode_parentT(Node,Anc) <- protein_tree_node(Node,_,_,L,_,_),protein_tree_node(Anc,_,_,LA,RA,_),L>LA,L<RA.

% TODO: cumulative distance. Should be easy with aggregate query and nested set

:- dynamic rdb_handle/1.
getrdb(Rdb):-
        nb_getval(rdb,Rdb).
setrdb(Rdb):-
        nb_setval(rdb,Rdb).


/** <module> Mapping between phylo_db and Ensembl Compara

  ---+ Synopsis

  ==
  :- use_module(bio(io)).
  :- use_module(bio(bioprolog_util),[solutions/3]).
  :- use_module(bio(rdb_util)).
  :- use_module(bio(genome_sqlmap_enscore)).
  :- use_module(bio(phylo_sqlmap_enscompara)).
  :- use_module(bio(phylo_db)).
  :- use_module(bio(genome_db)).
  
  demo:-
          initdb(homo_sapiens_core,Rdb),
          init_walk(Rdb,'BRCA1').
  
  % note that we only connect to one database.
  % we assume that both databases are on the same mysql
  % server, and that the tables in the compara database
  % can be accessed from the homo_sapiens database via
  % table name prefixing
  initdb(Db,Rdb):-
  	rdb_connect(Rdb,Db),
          sqlbind(phylo_db:phylonode_parent/2-Db),
          sqlbind(phylo_db:phylonode_branchlen/2-Db),
          % ensure the sql tables are prefixed by db name
          assert(sql_compiler:schema_dbname(enscompara,ensembl_compara_52)). % change with each release..
  
  %% init_walk(+Rdb,+Symbol)
  % given a human gene symbol, start the walk up the tree
  init_walk(Rdb,Symbol):-
          % package this as one sql query
  	rdb_query(Rdb,
                    Node,
                    (   gene_symbol(G,Symbol),
                        gene_translation(G,P),
                        phylonode_xref(Node,P))),
          walktree(Rdb,Node,0).
  
  %% walktree(+Rdb,+Node,+Dist)
  % iteratively walk up the tree, showing cumulative distance plus
  % all leafs below current node. This is illustrates a combination
  % of interwoven prolog and relational db calls
  walktree(Rdb,Node,Dist):-
          phylonode_branchlen(Node,BranchLen), % call db
          phylonode_parent(Node,Parent),       % call db
          !,
          Dist2 is Dist + BranchLen,
          format('Distance: ~w~n',[Dist2]),
          rdb_findall(Rdb,        % 1 db query
                     Tax-P,
                      (   phylonode_parentT(Sib,Parent),
                          phylonode_xref(Sib,P),
                          phylonode_taxon(Sib,Tax)),
                      PTaxPairs),
          maplist(writeln,PTaxPairs),
          nl,
          walktree(Rdb,Parent,Dist2).
  % we've reached the top
  walktree(_,Node,Dist):-
          format('Root: ~w ~w~n',[Node,Dist]),
          !.
  
          
          
    ==

    command line:

  ancestors of BRCA1
  ==
  blip-sql -r enscore/homo_sapiens_core -assert "sql_compiler:schema_dbname(enscompara,ensembl_compara_52)"  -u genome_sqlmap_enscore -u phylo_sqlmap_enscompara sql-map "(gene_symbol(G,'BRCA1'),gene_translation(G,P),phylonode_xref(Node,P),phylonode_parentT(Node,Anc))"
  ==


  
  ---+ Description

  This allows a SQL Database using the Enscore Compara schema to masquerade as phylo_db.pro
  predicates

  ---+ Mappings

  See source for details. The following predicates are mapped:

  * phylonode_parent
  * phylonode_parentT
  * phylonode_taxon
  * phylonode_xref

  
  ---+ More examples

  
  
  ---+ TODO


  ---+ Debug Codes

  * sql
  * sql_compiler
  
  ---+ See Also

    http://www.ensembl.org/info/docs/api/compara/compara_schema.html#protein_tree_node
  
  @author Chris Mungall
  @version  $Revision: 1.4 $
  @see schema_enscore44.pro, ../sql/odbc_setup.txt, sql_compiler.pro, plterm_to_sqlterm/3
  @license LGPL

  */
