
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

initdb(Db,Rdb):-
	rdb_connect(Rdb,Db),
        sqlbind(phylo_db:phylonode_parent/2-Db),
        sqlbind(phylo_db:phylonode_branchlen/2-Db),
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

        
        
