/* -*- Mode: Prolog -*- */


:- module(seqfeature_sqlmap_chado,
          [
          ]).

:- use_module(bio(sql_compiler)).
:- use_module(bio(rdb_util)).
:- use_module(bio(ontol_db),[]).
:- use_module(bio(metadata_db),[]).
:- use_module(bio(seqfeature_db),[]).

:- load_schema_defs(bio('sql_schema/schema_chado')).

:- multifile
	system:term_expansion/2.

dbxref_iid_id(DBXrefID,X) <-
  dbxrefd(DBXrefID,_,_,_,_,_,X). 

%feature0(InternalID,ID,Name) <-
%  feature(InternalID,DBXrefID,_,Name,_,_,_,_,_,_,_,_,_),
%  dbxref_iid_id(DBXrefID,ID).
feature0(ID,ID,Name) <-
  feature(ID,_,_,Name,_,_,_,_,_,_,_,_,_).


% PROBLEM! TODO! feature/N has meaning in both sql schema and prolog schema. former takes priority??
%:- abolish(seqfeature_db:feature/3).
%=seqfeature_db:feature(F,N,T) <- entity_label(F,N),feature_type(F,T).  % TODO leftjoin??

seqfeature_db:feature(ID) <- feature0(_InternalID,ID,_N).

seqfeature_db:feature_type(ID,T) <-
  feature0(InternalID,ID,_),
  feature(InternalID,_,_,_,_,_,_,_,TypeIID,_,_,_,_),
  cvterm(TypeIID,_,T,_,_,_,_).

:- abolish(seqfeature_db:implied_feature_type/3).
seqfeature_db:implied_feature_type(ID,TP,TC) <-
  feature0(InternalID,ID,_),
  feature(InternalID,_,_,_,_,_,_,_,ChildTypeIID,_,_,_,_),
  cvterm(IsaIID,_,is_a,_,_,_,_),
  cvterm_relationship(_,IsaIID,ChildTypeIID,ParentTypeIID),
  cvterm(ChildTypeIID,_,TC,_,_,_,_),
  cvterm(ParentTypeIID,_,TP,_,_,_,_).

:- abolish(seqfeature_db:implied_feature_type/2).
seqfeature_db:implied_feature_type(ID,TP) <-
  feature0(InternalID,ID,_),
  feature(InternalID,_,_,_,_,_,_,_,ChildTypeIID,_,_,_,_),
  cvterm(IsaIID,_,is_a,_,_,_,_),
  cvterm_relationship(_,IsaIID,ChildTypeIID,ParentTypeIID),
  cvterm(ParentTypeIID,_,TP,_,_,_,_).

% TODO: move?
/*
ontol_db:restrictionN(X,Rel,Y) <-
  cvterm(XIID,_,X,_,_,_,_),
  cvterm(YIID,_,X,_,_,_,_),
  cvterm(RelIID,_,Rel,_,_,_,_), % TODO - use actual ID
  cvterm_relationship(_,RelIID,XIID,YIID).
*/  

%seqfeature_db:feature(ID,N,T) <-
%  feature0(InternalID,ID,N),
%  feature(InternalID,_,_,_,_,_,_,_,TypeIID,_,_,_,_),
%  cvterm(TypeIID,_,T,_,_,_,_).

seqfeature_db:featureprop(FID,Prop,Val) <-
  feature0(InternalFID,FID,_),
  featureprop(_,InternalFID,TypeIID,Val,_),
  cvterm(TypeIID,_,Prop,_,_,_,_).

seqfeature_db:feature_relationship(SID,OID,T,Rank) <-
  feature0(InternalSID,SID,_),
  feature0(InternalOID,OID,_),
  feature_relationship(_,InternalSID,InternalOID,TypeIID,_,Rank),
  cvterm(TypeIID,_,T,_,_,_,_).

seqfeature_db:featureloc(Feature,SrcFeature,Min,Max,Strand,Rank,Group,[]) <- % TODO: beg/end
  feature0(FeatureIID,Feature,_),
  feature0(SrcFeatureIID,SrcFeature,_),
  featureloc(_,FeatureIID,SrcFeatureIID,Min,_,Max,_,Strand,_,_,Group,Rank).

seqfeature_db:feature_residues(Feature,Seq) <-
  feature0(FeatureIID,Feature,_),
  feature(FeatureIID,_,_,_,_,Seq,_,_,_,_,_,_,_).

seqfeature_db:feature_seqlen(Feature,SeqLen) <-
  feature0(FeatureIID,Feature,_),
  feature(FeatureIID,_,_,_,_,_,SeqLen,_,_,_,_,_,_).

%ontol_db:class(ACC) <-
%  dbxref_iid_id(DBXrefID,ACC),
%  cvterm(_,_,_,_,DBXrefID,_,_).



:- dynamic rdb_handle/1.
getrdb(Rdb):-
        nb_getval(rdb,Rdb).
setrdb(Rdb):-
        nb_setval(rdb,Rdb).

% -------------------- TESTS --------------------
% regression tests to ensure behaviour of module is correct;
% lines below here are not required for module functionality

unittest(load(chado)=
      (seqfeature_bridge_from_chadosql:set_bridge_resource(cjmchado))/[]).

unittest(test(basic,
            [_=load(chado)],
            (   ensure_loaded(bio(seqfeature_db)),
                ensure_loaded(bio(seqfeature_bridge_from_chadosql)),
                forall(feature(_ID,N,T),
                       format('~w ~w~n',[N,T]))),
            true)).

/** <module>   Mapping between seqfeature_db and Chado Sequence Module


  ==
  :- use_module(bio(seqfeature_bridge_from_chadosql)).
  :- seqfeature_bridge_from_chadosql:set_bridge_resource(chado_dmel).

  demo:-
    gene(GID,GN),
    gene_transcript(GID,TID),
    transcript(TID,TN),
    feature_residues(TID,Seq),
    format('Gene:~w ~w ~w~n',[GN,TN,Seq]),
    fail.
  ==

  ---+ Description

  This allows a SQL Database using the Chado Schema to masquerade as seqfeature
  predicates

  See <http://www.gmod.org/schema> Chado

  Command line:
  
  ==
  blip-sql prolog-to-sql -u seqfeature_sqlmap_chado "feature(X)" -proj "x(X)"
  ==

  ---+ TODO

  ---+ Debug Codes

  * sql
  * sql_compiler
  
  ---+ See Also

  @author Chris Mungall
  @version  $Revision: 1.4 $
  @see schema_chado.pro, ../sql/odbc_setup.txt, feature/1, sql_compiler.pro, plterm_to_sqlterm/3, seqfeature_sqlmap_chado_exposed_ids.pro
  @license LGPL

  */
