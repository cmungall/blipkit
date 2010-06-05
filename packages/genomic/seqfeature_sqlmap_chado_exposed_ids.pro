/* -*- Mode: Prolog -*- */


:- module(seqfeature_sqlmap_chado_exposed_ids,
          [
          ]).

:- use_module(bio(sql_compiler)).
:- use_module(bio(rdb_util)).
:- use_module(bio(bioprolog_util),[solutions/3]).
:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db),[]).
:- use_module(bio(seqfeature_db),[]).

:- load_schema_defs(bio('sql_schema/schema_chado')).

:- multifile
	system:term_expansion/2.

feature0(ID,ID,Name) <-
  feature(ID,_,_,Name,_,_,_,_,_,_,_,_,_).

featureloc0(FeatureIID,SrcFeatureIID,Min,Max,Strand) <-
  featureloc(_,FeatureIID,SrcFeatureIID,Min,_,Max,_,Strand,_,_,Group,Rank).
          
seqfeature_db:feature(ID) <- feature0(ID,_,_).

%:- abolish seqfeature_db:feature_label/2.
seqfeature_db:feature_label(ID,N) <- feature0(ID,_,N).

seqfeature_db:feature_type(IID,T) <-
  feature(IID,_,_,_,_,_,_,_,TIID,_,_,_,_),
  cvterm(TIID,_,T,_,_,_,_).



% TODO: implied_feature_type method depends on arity - make this more flexible

% this assumes the cvtermpath has been built... see below for another way..
seqfeature_db:implied_feature_type(ID,TP,TC) <-
  feature0(InternalID,ID,_),
  feature(InternalID,_,_,_,_,_,_,_,ChildTypeIID,_,_,_,_),
  cvterm(IsaIID,_,is_a,_,_,_,_),
  cvterm_relationship(_,IsaIID,ChildTypeIID,ParentTypeIID),
  cvterm(ChildTypeIID,_,TC,_,_,_,_),
  cvterm(ParentTypeIID,_,TP,_,_,_,_).

seqfeature_db:xxximplied_feature_type(ID,TP) <-
  feature0(InternalID,ID,_),
  feature(InternalID,_,_,_,_,_,_,_,ChildTypeIID,_,_,_,_),
  cvterm(IsaIID,_,is_a,_,_,_,_),
  cvterm_relationship(_,IsaIID,ChildTypeIID,ParentTypeIID),
  cvterm(ParentTypeIID,_,TP,_,_,_,_).

% this is a bit hacky.. ideally we could let sql_compiler figure this out
sql_compiler:view(implied_feature_type(X,IFTN),
     (   
         feature_type(X,AFTN),
         in(AFTN,AFTNs)
     )):-
        (   nonvar(IFTN)
        ->  debug(seqfeature_sql,'Expanding: ~w',[IFTN]),
	    solutions(AFTN,
                      (	  class(IFT,IFTN),
                          subclassRT(AFT,IFT),
                          class(AFT,AFTN)),
                      AFTNs)
        ;   throw(sqlmap('Implied feature types must be ground'))).

% generate predicates of the form a_intron/1 etc
% TODO: move to separate module?
sql_compiler:view(Head,
     (   
         feature_type(X,AFTN),
         in(AFTN,AFTNs)
     )):-
	nonvar(Head),
	Head =.. [IFTN_A,X],
	atom_concat('a_',IFTN,IFTN_A),
        class(_,IFTN),		% exact synonyms too?
	debug(seqfeature_sql,'Expanding: ~w',[IFTN]),
        solutions(AFTN,
		  (   class(IFT,IFTN),
		      subclassRT(AFT,IFT),
		      class(AFT,AFTN)),
		  AFTNs).


% TODO: move?
ontol_db:restrictionN(X,Rel,Y) <-
  cvterm(XIID,_,X,_,_,_,_),
  cvterm(YIID,_,Y,_,_,_,_),
  cvterm(RelIID,_,Rel,_,_,_,_), % TODO - use actual ID
  cvterm_relationship(_,RelIID,XIID,YIID).


%seqfeature_db:feature(ID,N,T) <-
%  feature0(InternalID,ID,N),
%  feature(InternalID,_,_,_,_,_,_,_,TypeIID,_,_,_,_),
%  cvterm(TypeIID,_,T,_,_,_,_).

seqfeature_db:featureprop(FID,Prop,Val) <-
  feature0(InternalFID,FID,_),
  featureprop(_,InternalFID,TypeIID,Val,_),
  cvterm(TypeIID,_,Prop,_,_,_,_). % use the name and hope its unique

seqfeature_db:feature_relationship(SID,OID,T,Rank) <-
  feature_relationship(_,SID,OID,TypeIID,_,Rank),
  cvterm(TypeIID,_,T,_,_,_,_). % use the name of the relation

seqfeature_db:featureloc(Feature,SrcFeature,Min,Max,Strand,Rank,Group) <- % TODO: beg/end
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

j_before_j(J1,J2,1) <- J1 < J2.
%j_before_j(J1,J2,1) <- J1 < J2.
%j_before_j(J1,J2,-1) <- J1 > J2.

%seqfeature_db:starts_before_start_of2(F1,F2) <-
% starts_at(F1,Seq,Ori,Start1),
% starts_at(F2,Seq,Ori,Start2),
% (   Ori = -1
% ;   Ori = 1),
% j_before_j(Start1,Start2,Ori).

seqfeature_db:starts_before_start_of(F1,F2) <-
 starts_at(F1,Seq,1,Start1),
 starts_at(F2,Seq,1,Start2),
 Start1 < Start2.	
seqfeature_db:starts_before_start_of(F1,F2) <-
 starts_at(F1,Seq,-1,Start1),
 starts_at(F2,Seq,-1,Start2),
 Start1 > Start2.	

seqfeature_db:starts_before_end_of(F1,F2) <-
 starts_at(F1,Seq,1,Start1),
 ends_at(F2,Seq,1,End2),
 Start1 < End2.
seqfeature_db:starts_before_end_of(F1,F2) <-
 starts_at(F1,Seq,-1,Start1),
 ends_at(F2,Seq,-1,End2),
 Start1 > End2.

seqfeature_db:ends_after_start_of(F1,F2) <-
 ends_at(F1,Seq,1,End1),
 starts_at(F2,Seq,1,Start2),
 End1 > Start2.
seqfeature_db:ends_after_start_of(F1,F2) <-
 ends_at(F1,Seq,-1,End1),
 starts_at(F2,Seq,-1,Start2),
 End1 < Start2.

starts_at(F,Seq,1,X) <-
  featureloc0(F,Seq,X,_,1).
starts_at(F,Seq,-1,X) <-
  featureloc0(F,Seq,_,X,-1).

ends_at(F,Seq,1,X) <-
  featureloc0(F,Seq,_,X,1).
ends_at(F,Seq,-1,X) <-
  featureloc0(F,Seq,X,_,-1).


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

/** <module>   Mapping between seqfeature_db and Chado Sequence Module [internal IDs exposed]

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

  This allows a SQL Database using the Chado Schema to masquerade as
  seqfeature predicates. It differs from seqfeature_sqlmap_chado.pro
  in that it exposes database internal IDs as the keys to feature/1
  etc. This makes it faster, but care must be taken combining datasets
  from multiple dbs


  See <http://www.gmod.org/schema> Chado

  Command line:

  ==
blip-sql -debug sql -debug sql_compiler -u seqfeature_sqlmap_chado_exposed_ids -r rdb/flybase sql-map "feature_label(ID,dpp)"
  ==
  
  ==
  blip-sql prolog-to-sql -u seqfeature_sqlmap_chado "feature(X)" -proj "x(X)"
  ==

  Using genome_db model; e.g. gene/1. We must use genome_bridge_from_seqfeature.pro to de-reify
  
  ==
  blip-sql -u seqfeature_sqlmap_chado_exposed_ids -u genome_bridge_from_seqfeature -r rdb/flybase prolog-to-sql "(genome_db:gene_label(X,dpp))"
  ==

  ==
  blip-sql -debug sql  -u seqfeature_sqlmap_chado_exposed_ids -u genome_bridge_from_seqfeature -r rdb/flybase prolog-to-sql -proj TN "feature_label(G,dpp),genome_db:gene_transcript(G,T),feature_label(T,TN),genome_db:exon_transcript_order(X,T,Rank)"
  ==

  ---++ Ontology Based Query Expansion
  
  Load SO using load_bioresource/2, then use subclassRT/2 to expand query for 'gene'
  ==
  blip-sql -r so -u seqfeature_sqlmap_chado_exposed_ids -u genome_bridge_from_seqfeature "implied_feature_type(F,gene),feature_label(F,dpp)" 
  ==
  In this case it's a bit uneccessary as there is only one feature 'dpp', and FlyBase doesn't use subtypes of gene.

  Currently this module auto-expands to predicates named a_<type>/1; e.g.:
  ==
  blip-sql -r so -u seqfeature_sqlmap_chado_exposed_ids -u genome_bridge_from_seqfeature "a_gene(F),feature_label(F,dpp)" 
  ==
  This behavior may change:
  * choice between prefixing and no prefixes (prefixing allows additional inference for the core predicate)
  TODO: customizable; no point expanding to predicate that is never used

  currently this loops:
  ==
  blip-sql -u genome_inference -debug seqfeature_sql -r so -u seqfeature_sqlmap_chado_exposed_ids -u genome_bridge_from_seqfeature prolog-to-sql "intron(F)"
  ==
  
  ---++ Range Operations
  
  ==
  blip-sql -u seqfeature_db -u seqfeature_sqlmap_chado_exposed_ids -r rdb/go prolog-to-sql "feature_label(G,dpp),feature_range(G,GR)"
  ==
  
  ---+ TODO

  translate Min and Max to Beg and End  (multiply by strand)
  
  ---+ Debug Codes

  * sql
  * sql_compiler
  
  ---+ See Also

  @author Chris Mungall
  @version  $Revision: 1.4 $
  @see schema_chado.pro, ../sql/odbc_setup.txt, feature/1, sql_compiler.pro, plterm_to_sqlterm/3, seqfeature_sqlmap_chado.pro
  @license LGPL

  */
