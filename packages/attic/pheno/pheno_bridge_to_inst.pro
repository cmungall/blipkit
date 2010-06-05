/* -*- Mode: Prolog -*- */


:- module(pheno_bridge_to_inst,
          []).

%:- use_module(bio(ontol_db)).
:- use_module(bio(pheno_db)).

% can we constrain this mapping more?
% annotation? more specific relation?
% note that inference will produce spurious 'data'
% - eg return wild type part_of relations
% - may be better with explicit genotype+environment -> phenotype assocs

% G = gene instance (genotype/allele - simplification)
ontol_db:inst(G,N):-                         genotype(G,N).
ontol_db:inst_of(G,'OBR:genotype'):-           genotype(G,_).
ontol_db:inst_rel(G,'OBO_REL:part_of',P):-   genotype_phenotype(G,P).
ontol_db:inst_rel(G,'OBO_REL:has_part',F):-  genotype_feature(G,F).

% P = organism instance (phenotype); OC = phenotype/organism class
ontol_db:inst(P,P):-      phenotype(P).
ontol_db:inst_of(P,OC):-  genotype_phenotype(G,P), genotype_organism(G,OC).

% PC = bearer instance (phenotype character)
ontol_db:inst(PC,''):-                           phenochar_bearer_class(PC,_).
ontol_db:inst_of(PC,BC):-                        phenochar_bearer_class(PC,BC).
ontol_db:inst_rel(PC,'OBO_REL:part_of',P):-      phenotype_phenochar(P,PC).
ontol_db:inst_rel(PC,'OBO_REL:has_quality',Q):-  phenochar_quality(PC,Q).

% Q = quality instance; QC = quality class
ontol_db:inst(Q,''):-       quality_class(Q,_).
ontol_db:inst_of(Q,QC):-       quality_class(Q,QC).
ontol_db:inst_rel(Q,R,To):-    quality_qualifier(Q,R,To). % should be instance!

% assuming the CW assumption, the listed alleles can be regarded as
% implicated in causality
ontol_db:inst_of(M,''):-
        phenotype_manifestation(M,_,_).
ontol_db:inst_of(M,'OBR:pathological_process'):-
        phenotype_manifestation(M,_,_).
ontol_db:inst_rel(M,'Pheno:has_cause',F):-
        phenotype_manifestation(M,_,G),genotype_feature(G,F).
ontol_db:inst_rel(M,'Pheno:has_outcome',Q):-
        phenotype_manifestation(M,P,_),phenotype_phenochar(P,PC),
        phenochar_quality(PC,Q).
% TODO - annotation model
ontol_db:inst_sv(M,'rdfs:comment',Desc,'xsd:string'):- phenotype_manifestation_desc(M,Desc).
/** <module>
  @author Chris Mungall
  @version  $Revision: 1.2 $
  @date  $Date: 2006/01/04 11:49:05 $
  @license LGPL

  ---+ Name
  ---++ pheno_bridge_to_inst
- view pheno_db as generic ontol instances

  ---+ Synopsis

  ==
  :- use_module(bio(pheno_bridge_to_inst)).

  ==

  ---+ Description

  each phenotype is a particular organism, with various parts
  
**/