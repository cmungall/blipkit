/* -*- Mode: Prolog -*- */


:- module(pheno_bridge_to_inst,
          []).

:- use_module(bio(ontol_db)).
:- use_module(bio(pheno_db)).

% can we constrain this mapping more?
% annotation? more specific relation?
% note that inference will produce spurious 'data'
% - eg return wild type part_of relations
% - may be better with explicit genotype+environment -> phenotype assocs

% A GPE statement is not a statement of fact about specific parts, qualities, insts
% It is a stronger statement, specificall about how mutation in genes gives
% rise to mutant phenotype

% P = organism instance (phenotype); OC = phenotype/organism class
ontol_db:inst(P,P):-      phenotype(P).
ontol_db:inst_of(P,OC):-  genotype_phenotype(G,P), genotype_organism(G,OC).

ontol_db:inst(G,N):-                         genotype(G,N).
ontol_db:inst_of(G,'SO:0000110'):-           genotype(G,_).
ontol_db:inst_rel(G,'OBO_REL:part_of',P):-   genotype_phenotype(G,P).

% G = genotype
ontol_db:inst(G,N):-                          genotype(G,N).
ontol_db:inst_of(P,'Pheno:sequence_variant_aggregate'):-
        phenotype(P).
ontol_db:inst_rel(P,'Pheno:gives_rise_to',PC):-     phenotype_phenochar(P,PC).
ontol_db:inst_rel(P,'OBO_REL:has_variant',F):-
        genotype_phenotype(G,P),
        genotype_feature(G,F).

%ontol_db:inst(E,N):-                              environment(E,N).
%ontol_db:inst_of(E,'Pheno:environment'):-         environment(E,_).
%ontol_db:inst_rel(,'Pheno:has_environment',P):-   environment_phenotype(G,P).

% PC = bearer instance (phenotype character)
ontol_db:inst_of(PC,BC):-                        phenochar_bearer_class(PC,BC).
ontol_db:inst_rel(PC,'OBO_REL:has_quality',Q):-  phenochar_quality(PC,Q).

% Q = quality instance; QC = quality class
ontol_db:inst_of(Q,QC):-       quality_class(Q,QC).
ontol_db:inst_rel(Q,R,To):-    quality_qualifier(Q,R,To). % should be instance!

                                



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