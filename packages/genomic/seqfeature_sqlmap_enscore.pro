/* -*- Mode: Prolog -*- */

:- module(seqfeature_sqlmap_enscore,
          [
          ]).

:- use_module(bio(sql_compiler)).
:- use_module(bio(rdb_util)).
:- use_module(bio(ontol_db),[]).
:- use_module(bio(metadata_db),[]).
:- use_module(bio(seqfeature_db),[]).

:- load_schema_defs(bio('sql_schema/schema_enscore44')).

:- multifile
	system:term_expansion/2.

% TODO - union views
%feature_i(InternalID,PublicID,Symbol,Type) <-
% (   Type=gene,gene(InternalID,_,_,_,_,_,_,X),gene_stable_id(InternalID,PublicID),xref(X,_,_,Symbol))
% ;           
% (   Type=exon,exon(InternalID),PublicID=InternalID,Symbol=InternalID)
% ;           
% (   Type=transcript,transcript(InternalID,_,_,_,_,_,_,X),transcript_stable_id(InternalID,PublicID),xref(X,_,_,Symbol)).

feature_i(InternalID,PublicID,Symbol,gene) <-
  gene(InternalID,_,_,_,_,_,_,X),gene_stable_id(InternalID,PublicID),xref(X,_,_,Symbol).

feature_i(ID,ID,ID,exon) <-
  exon(ID).

feature_i(InternalID,PublicID,Symbol,transcript) <-
  transcript(InternalID,_,_,_,_,_,_,X),transcript_stable_id(InternalID,PublicID),xref(X,_,_,Symbol).
          
:- abolish(seqfeature_db:feature/3).
seqfeature_db:feature(ID,N,T) <-
  feature_i(_,ID,N,T).

:- abolish(seqfeature_db:feature_relationship/4).
% no union as yet..
seqfeature_db:feature_relationshipx(SID,OID,part_of,Rank) <-
  feature_i(InternalSID,SID,_,exon),
  feature_i(InternalOID,OID,_,transcript),
  exon_transcript(InternalSID,InternalOID,Rank).

seqfeature_db:feature_relationship(SID,OID,part_of,0) <-
  transcript(InternalSID,InternalOID),
  transcript_stable_id(InternalSID,SID),
  feature_i(InternalOID,OID,_,gene).

:- dynamic rdb_handle/1.
getrdb(Rdb):-
        nb_getval(rdb,Rdb).
setrdb(Rdb):-
        nb_setval(rdb,Rdb).

% -------------------- TESTS --------------------
% regression tests to ensure behaviour of module is correct;
% lines below here are not required for module functionality

unittest:testdb(enscore(homo_sapiens_core)).
%unittest:testq(basic,brca1(G),(feature(G,'BRCA1',_))).
unittest:testq(basic,brca1(G,T),(feature(G,'BRCA1',_),seqfeature_db:feature_relationship(T,G))).
%unittest:testq(basic,brca1(G,T,TN),(feature(G,'BRCA1',_),seqfeature_db:feature_relationship(T,G),seqfeature_db:feature(T,TN,_))).

xunittest(load(enscore)=
      (   load_bioresource(enscore(homo_sapiens_core)))/[]).

xunittest(test(basic2,
            [_=load(enscore)],
            (   ensure_loaded(bio(seqfeature_db)),
                ensure_loaded(bio(seqfeature_bridge_from_enscoresql)),
                forall((feature(G,'BRCA1',_),feature_relationship(T,G),feature(T,TN,_)),
                       format('~w ~w ~w~n',[G,T,TN]))),
            true)).

unittest(test(basic,
            [],
            (   ensure_loaded(bio(sql_compiler)),
                ensure_loaded(bio(rdb_util)),
                ensure_loaded(bio(seqfeature_db)),
                ensure_loaded(bio(seqfeature_sqlmap_enscore)),
                %testdb(Db),
                rdb_connect(Rdb,homo_sapiens_core),
                forall(testq(basic,Proj,Goal),
                       (   writeln(trying(Proj)),
                           forall(rdb_query(Rdb,Proj,Goal),
                                  writeln(Proj)),
                           nl)),
                writeln(done)),
            true)).


/** <module>   
  @author Chris Mungall
  @version  $Revision: 1.4 $
  @date  $Date: 2006/03/25 01:57:15 $
  @license LGPL

  ---+ Name
%  seqfeature_bridge_from_enscoresql

  ---+ Synopsis

  ==
  :- use_module(bio(seqfeature_bridge_from_enscoresql)).
  :- seqfeature_bridge_from_enscoresql:set_bridge_resource(enscore_dmel).

  demo:-
    gene(GID,GN),
    gene_transcript(GID,TID),
    transcript(TID,TN),
    feature_residues(TID,Seq),
    format('Gene:~w ~w ~w~n',[GN,TN,Seq]),
    fail.
  ==

  ---+ Description

  This allows a SQL Database using the Enscore Schema to masquerade as seqfeature
  predicates

  */
