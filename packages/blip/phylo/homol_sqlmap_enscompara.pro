/* -*- Mode: Prolog -*- */

:- module(homol_sqlmap_enscompara,
          [
          ]).

:- use_module(bio(sql_compiler)).
:- use_module(bio(rdb_util)).
:- use_module(bio(metadata_db),[]).
:- use_module(bio(homol_db),[]).

:- load_schema_defs(bio('sql_schema/schema_enscompara')).

:- multifile
	system:term_expansion/2.

homol_db:homologset(S) <- homology(S).
homol_db:homologset_member(S,M) <- homology_member(S,MI),member(MI,M,_,_,_,_,_,_,_,_,_,_,_,_). % remember, member/2 will be confused with the builtin!
homol_db:homologset_member_taxon(S,M,T) <- homology_member(S,MI),member(MI,M,_,_,T,_,_,_,_,_,_,_,_,_). 
metadata_db:entity_label(M,L) <- member(_,M,_,_,_,_,_,_,_,_,_,_,_,L). 

/** <module> 

  ---+ Synopsis

find homologs of SHH:
==
blip-sql -u homol_db -u homol_sqlmap_enscompara -r enscore/ensembl_compara sql-map "homologous_to(X,'ENSG00000164690')"  
==

  ---+ Details

  This maps the extensional predicates homologset/1 homologset_member/2 and the intensional predicate honologset_member_taxon/3 to the ensembl compara schema

  entity_label/2 is also mapped (todo: determine if this will interfere)
  
*/


