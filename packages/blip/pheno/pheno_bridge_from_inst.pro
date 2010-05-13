/* -*- Mode: Prolog -*- */


:- module(pheno_bridge_from_inst,
          [
          ]).

:- use_module(bio(ontol_db)).
:- use_module(bio(pheno_db)).


% G = gene instance (genotype/allele - simplification)
pheno_db:genotype(G,N):-
        pheno_bridge_from_inst:genotype(G),
        inst(G,N).
pheno_db:genotype_phenotype(G,P):-
        inst_rel(G,'OBO_REL:part_of',P),
        pheno_bridge_from_inst:genotype(G).

% P = organism instance (phenotype); OC = phenotype/organism class
pheno_db:phenotype(P):- pheno_db:genotype_phenotype(_,P).
pheno_db:genotype_organism(G,OC):- pheno_db:genotype_phenotype(G,P),inst_of(P,OC).

pheno_db:phenotype_phenochar(P,PC):-
        pheno_db:phenotype(P),
        inst_rel(PC,'OBO_REL:part_of',P).

pheno_db:phenochar_bearer_class(PC,BC):-
        pheno_db:phenotype_phenochar(_,PC),
        inst_of(PC,BC).

pheno_db:phenochar_quality(PC,Q):-
        inst_rel(PC,'OBO_REL:has_quality',Q).

pheno_db:quality_class(Q,QC):- pheno_db:phenochar_quality(_,Q),inst_of(Q,QC).
pheno_db:quality_qualifier(Q,R,To):- inst_rel(Q,R,To).

genotype(G):- inst_of(G,'SO:0000110').


/** <module>   
  @author Chris Mungall
  @version  $Revision: 1.3 $
  @date  $Date: 2005/07/02 00:40:21 $
  @license LGPL

  DEPRECATED

  ==
  :- use_module(bio(pheno_bridge_from_inst)).
  ==

  */
