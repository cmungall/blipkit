:- module(ontol_sqlmap_obd_denorm_annot,[]).

:- use_module(bio(sql_compiler)).
:- use_module(bio(rdb_util)).
:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).
:- use_module(bio(curation_db)). % move?

:- load_schema_defs(bio('sql_schema/schema_obd_denorm_annot')).

:- multifile
	system:term_expansion/2.

curation_db:curation_statement(Cu,S,R,O) <- annotation(Cu,S,R,O,_,_,_,_,_).
ontol_db:inst_rel(S,R,O) <- annotation(_,S,R,O,_,_,_,_,_).

:- dynamic rdb_handle/1.
getrdb(Rdb):-
        sql_compiler:sqlschema_connection(obd_annot,Rdb).

% blip-obd -debug sql -r obd_annot/annotation_hprd -u ontol_sqlmap_obd_denorm_annot -r obd/obd_ncbi_gene_9606 ontol-subset -n MMP9
