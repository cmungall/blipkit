:- module(homol_sqlmap_go,[]).

:- use_module(bio(sql_compiler)).
:- use_module(bio(rdb_util)).
:- use_module(bio(homol_db),[]).
:- use_module(bio(metadata_db),[]).

:- load_schema_defs(bio('sql_schema/schema_go')).
:- [bio(ontol_sqlmap_go_util)].

:- multifile
	system:term_expansion/2.

homol_db:homologset(H) <-
        homolset(H,_,_,_,_,_,_).

homol_db:homologset_member(H,G) <-
        gene_product_homolset(_,GI,H),product0(GI,G,_).

/** <module> Maps homol_db predicates to GO database homolset tables

  ---+ Synopsis

  ==
  :- use_module(bio(homol_bridge_from_gosql)).
  :- homol_bridge_from_gosql:set_bridge_resource(go_dmel).

  demo:-
    gene(GID,GN),
    gene_transcript(GID,TID),
    transcript(TID,TN),
    feature_residues(TID,Seq),
    format('Gene:~w ~w ~w~n',[GN,TN,Seq]),
    fail.
  ==

  ---+ Description

  This allows a SQL Database using the Go Schema to masquerade as homol
  predicates

  See <http://www.gmod.org/schema> Go

  Command line:
  
  ==
  blip-sql prolog-to-sql -u homol_sqlmap_go "feature(X)" -proj "x(X)"
  ==

  @see go_database_tutorial.txt
  
  */



