
/* -*- Mode: Prolog -*- */


:- module(pheno_bridge_from_starcode,
          []).

:- use_module(bio(ontol_db)).
:- use_module(bio(pheno_db)).

flatten_skolem(Sk,ID):-
        Sk=..L,
        concat_atom(L,'__',ID).

a2gt(A,Gt):-
        flatten_skolem(gt(A),Gt).
p2m(P,M):-
        flatten_skolem(pm(P),M).
p2pc(P,PC):-
        flatten_skolem(pc(P),PC).

so_gene_id(C):- class(C,gene).

pheno_db:genotype(Gt,N):-
        allele_symbol(A,N),     % all are single-allele
        a2gt(A,Gt).
pheno_db:genotype_feature(Gt,A):-
        allele_symbol(A,_),     % all are single-allele
        a2gt(A,Gt).
seqfeature_db:feature(Gn,N,T):-
        so_gene_id(T),
        gene_symbol(Gn,N).
seqfeature_db:feature(A,N,T):-
        so_gene_id(T),
        allele_symbol(A,N).
seqfeature_db:feature_relationship(A,Gn,allele_of,0):-
        gene_allele(Gn,A).
pheno_db:genotype_organism(Gt,'NCBITaxon:7227'):-
        allele_symbol(A,_),
        a2gt(A,Gt).
pheno_db:phenotype_manifestation(M,P,Gt):-
        allele_phenotype(A,P),
        a2gt(A,Gt),
        p2m(P,M).
pheno_db:phenotype_manifestation_desc(M,Desc):-
        phenotype_desc(P,Desc),
        p2m(P,M).
pheno_db:phenotype_class(P,C):-
        phenotypic_class(P,CN,_BackgroundGt), % todo
        class(C,CN).
pheno_db:phenotype_phenochar(P,PC):-
        allele_phenotype(_,P),
        p2pc(P,PC).
pheno_db:phenochar_bearer_class(PC,C):-
        manifest_in(P,CN,_BackgroundGt), % todo
        class(C,CN),
        p2pc(P,PC).
/** <module>
  @author Chris Mungall
  @version  $Revision$
  @date  $Date$
  @license LGPL

  ---+ Name
  ---++ pheno_bridge_from_starcode
- old to new

  ---+ Synopsis

  ==
  :- use_module(bio(pheno_bridge_from_starcode)).

  ==

  ---+ Description

  one-off code - migrates old to new schema

  remember to load fly_anat! and SO

  blip -i starcode_fb.pro io-convert -u pheno_bridge_from_starcode -r fly_anatomy -r fly_development -r pato -r so -to pheno_db:pro > nupheno_fb.pro

  
**/