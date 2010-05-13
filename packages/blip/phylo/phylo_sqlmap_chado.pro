/* -*- Mode: Prolog -*- */


:- module(phylo_sqlmap_chado,
          [
          ]).

:- use_module(bio(sql_compiler)).
:- use_module(bio(rdb_util)).
:- use_module(bio(ontol_db),[]).
:- use_module(bio(metadata_db),[]).
:- use_module(bio(phylo_db),[]).

:- load_schema_defs(bio('sql_schema/schema_chado')).

:- multifile
	system:term_expansion/2.

dbxref_iid_id(DBXrefID,X) <-
  dbxrefd(DBXrefID,_,_,_,_,_,X). 


metadata_db:entity_label(Node,Name) <- phylonode(Node,_,_,_,_,_,_,Name,_).
phylo_db:phylonode_branchlen(Node,Len) <- phylonode(Node,_,_,_,_,_,_,_,Len).
phylo_db:phylonode_parent(Node,P) <- phylonode(Node,_,P,_,_,_,_,_,_).


:- dynamic rdb_handle/1.
getrdb(Rdb):-
        nb_getval(rdb,Rdb).
setrdb(Rdb):-
        nb_setval(rdb,Rdb).

% -------------------- TESTS --------------------
% regression tests to ensure behaviour of module is correct;
% lines below here are not required for module functionality

unittest(load(chado)=
      (phylo_bridge_from_chadosql:set_bridge_resource(cjmchado))/[]).

unittest(test(basic,
            [_=load(chado)],
            (   ensure_loaded(bio(phylo_db)),
                ensure_loaded(bio(phylo_bridge_from_chadosql)),
                forall(feature(_ID,N,T),
                       format('~w ~w~n',[N,T]))),
            true)).
/** <module>   
  @author Chris Mungall
  @version  $Revision: 1.4 $
  @date  $Date: 2006/03/25 01:57:15 $
  @license LGPL

  ---+ Name
%  phylo_bridge_from_chadosql

  ---+ Synopsis

  ==
  :- use_module(bio(phylo_bridge_from_chadosql)).
  :- phylo_bridge_from_chadosql:set_bridge_resource(chado_dmel).

  demo:-
    gene(GID,GN),
    gene_transcript(GID,TID),
    transcript(TID,TN),
    feature_residues(TID,Seq),
    format('Gene:~w ~w ~w~n',[GN,TN,Seq]),
    fail.
  ==

  ---+ Description

  This allows a SQL Database using the Chado Schema ( phylogeny module
  ) to masquerade as phylo predicates

  See <http://www.gmod.org/schema> Chado

  Command line:
  
  ==
  blip-sql prolog-to-sql -u phylo_sqlmap_chado "feature(X)" -proj "x(X)"
  ==
  
  */
