:- module(rna_sqlmap_rna,
	  []).


:- use_module(bio(sql_compiler)).
:- use_module(bio(rdb_util)).
:- use_module(rna_db).


:- load_schema_defs(bio('sql_schema/schema_rna')).