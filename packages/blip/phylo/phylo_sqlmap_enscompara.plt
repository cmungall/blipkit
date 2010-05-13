/* -*- Mode: Prolog -*- */

:- use_module(phylo_sqlmap_enscompara).
:- use_module(phylo_db).
:- use_module(bio(genome_db)).
:- use_module(bio(genome_sqlmap_enscore)).
:- use_module(bio(sql_compiler)).
:- use_module(bio(rdb_util)).

:- op(1100,fx,testq).
:- op(1000,xfy,where).

sql_compiler:schema_dbname(enscompara,ensembl_compara_52).

test_query(Proj,Goal) :-
        writeln(Proj where Goal),
        catch(plterm_to_sqlterm(Proj,Goal,SqlTerm),
              E,
              (   writeln(E),
                  fail)),
        writeln(SqlTerm),
        sqlterm2atom(SqlTerm,Sql),
        writeln(Sql),
        format('** SUCCESS: ~w~n',[Proj]),
        nl.

test_query_exec(Rdb,Proj,Goal) :-
        writeln(Proj where Goal),
        forall(rdb_query(Rdb,Proj,Goal),
               writeln(Proj)),
        format('** SUCCESS: ~w~n',[Proj]),
        nl.

testq brca1_phylonode(Node,Tax) where gene_symbol(G,'BRCA1'),gene_polypeptide(G,P),phylonode_xref(Node,P),phylonode_taxon(Node,Tax).
testq brca1_phylonode2(Node,Parent) where gene_symbol(G,'BRCA1'),gene_polypeptide(G,P),phylonode_xref(Node,P),phylonode_parent(Node,Parent).
testq brca1_anc(Anc) where gene_symbol(G,'BRCA1'),gene_polypeptide(G,P),phylonode_xref(Node,P),phylonode_parentT(Node,Anc).
testq brca1_tree1(G,P,Node,Tree) where gene_symbol(G,'BRCA1'),
                                               gene_polypeptide(G,P),
                                               phylonode_xref(Node,P),
                                               phylonode_tree(Node,Tree).
% this is too inefficient
%testq brca1_treesib(P2) where 
%                                         phylonode_xref(Node,'ENSP00000350283'), % BRCA1
%                                         phylonode_tree(Node,Tree),
%                                         phylonode_tree(Node2,Tree),
%                                         phylonode_xref(Node2,P2).
%                                         gene_polypeptide(G2,P2),
%                                         gene_symbol(G2,S2).


:- begin_tests(enscompara, []).

test(nodb):-
        findall(Proj-Where,
                (   (   testq Proj where Where),
                    \+ test_query(Proj,Where)),
                Fails),
        (   Fails=[]
        ->  format('passed~n')
        ;   format('FAILED:~n'),
            maplist(writeln,Fails),
            fail).


:- end_tests(enscompara).


:- begin_tests(withdb, []).


% Requires ODBC to be configured
test(withdb) :-
        writeln(connecting),
        rdb_connect(Rdb,enscore(homo_sapiens_core)),
        format('connected: h=~w~n',[Rdb]),
        findall(Proj-Where,
                (   (   testq Proj where Where),
                    \+ test_query_exec(Rdb,Proj,Where)),
                Fails),
        (   Fails=[]
        ->  format('PASSED ALL~n')
        ;   format('FAILED:~n'),
            maplist(writeln,Fails),
            fail).


:- end_tests(withdb).


