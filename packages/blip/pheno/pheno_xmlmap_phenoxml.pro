/* -*- Mode: Prolog -*- */

:- module(pheno_xmlmap_phenoxml,[]).

:- use_module(bio(xml_transform)).
:- use_module(bio(pheno_db)).
:- use_module(bio(metadata_db)).
:- use_module(bio(ontol_db)).

io:xml_to_preds(phenoxml,XML,PL):-
        apply_xmlpred(pheno_xmlmap_phenoxml,XML,PL).

xmlpred(phenoset,_,[],
        [translate(genotype),
         translate(genetic_feature),
         translate(phenotype,_),
         translate(phenotype_manifestation)
         ]).
xmlpred(genetic_feature,_,[seqfeature_db:feature(F),
                           seqfeature_db:feature_type(F,T),
                           metadata_db:entity_label(F,N)],
        [
         let(F=att(id)),
         let(N=name),
         translate(typeref,class(T))
        ]).
xmlpred(genotype,_,[genotype(Gt),metadata_db:entity_label(Gt,N),genotype_organism(Gt,Org)],
        [
         let(Gt=att(id)),
         let(N=name),
         prolog(M=in_genotype(Gt)),
         let(Org=att(organism)),
         translate(genotype_feature_link,M)
        ]).
xmlpred(organism,in_genotype(Gt),genotype_organism(Gt,Org),
        [
         let(Org=att('.'))
        ]).
xmlpred(genotype_feature_link,in_genotype(Gt),genotype_feature(Gt,F),
        [
         let(F=att(feature))
        ]).
%todo: typeref
xmlpred(phenotype_manifestation,_,phenotype_manifestation(M,P,Gt),
        [
         prolog(genid(phenotype_annotation,M)),  % assume no ID
         let(Gt=[manifest_in,att(genotype)]),
         translate(provenance,in_pm(M)),
         translate(annotation_provider,in_pm(M)),
         translate([manifest_in,typeref],in_pm(M)),
         translate(phenotype,in_phenotype(P))
        ]).
%xmlpred(provenance,in_pm(M),phenotype_manifestation_provenance(M,Pub),
%        let(Pub=att(about))).
xmlpred(provenance,in_pm(M),metadata_db:entity_source(M,Pub),
        let(Pub=att(about))).
xmlpred(annotation_provider,in_pm(M),metadata_db:entity_resource(M,X),
        let(X=att(about))).
%xmlpred(annotation_provider,in_pm(M),phenotype_manifestation_provider(M,X),
%        let(X=att(about))).

xmlpred(phenotype,in_phenotype(P),phenotype(P),
        [
         prolog(genid(organism_instance,P)),  % assume no ID
         translate(phenotype_character,in_phenotype(P))
        ]).
xmlpred(phenotype_character,in_phenotype(P),phenotype_phenochar(P,PC),
        [
         prolog(genid(quality_bearer_instance,PC)),
         translate(description,in_phenochar(PC)),
         translate(bearer,in_phenochar(PC)),
         translate(quality,in_phenochar(PC))
        ]).
xmlpred(description,in_phenochar(PC),phenochar_desc(PC,Desc),
        [
         let(Desc='.')
        ]).
xmlpred(bearer,in_phenochar(PC),phenochar_bearer_class(PC,Class),
        [
         translate(typeref,class(Class))
        ]).
xmlpred(quality,in_phenochar(PC),[phenochar_quality(PC,Q),quality_class(Q,Class)],
        [
         prolog(genid(quality_instance,Q)),
         translate(typeref,class(Class)),
         translate(related_entity,in_quality(Q)),
         translate(temporal_qualifier,in_quality(Q)),
         translate(modifier,in_quality(Q))
        ]).
xmlpred(temporal_qualifier,in_quality(Q),quality_qualifier(Q,Rel,To),
        [
         let(Rel=att(relation)),
         translate([time_range,typeref],class(To))
        ]).
xmlpred(modifier,in_quality(Q),quality_qualifier(Q,modifier,M),
        [
         translate(typeref,class(M))
        ]).
xmlpred(related_entity,in_quality(Q),quality_related_entity(Q,C),
        [
         translate(typeref,class(C))
        ]).


% TODO - post-coord
xmlpred(typeref,class(Class),Facts,
        [
         prolog(genid(class,AnonClass)),
         let(Genus=att(about)),
         mvlet(Qualifiers=qualifier),
         translate(qualifier,in_anon(AnonClass)),
         prolog((   Qualifiers=[]
                ->  Class=Genus,
                    Facts=[]
                ;   Class=AnonClass,
                    Facts=[ontol_db:genus(AnonClass,Genus),
                           ontol_db:is_anonymous(AnonClass)]))
        ]).
xmlpred(qualifier,in_anon(AnonClass),[ontol_db:differentium(AnonClass,Rel,To)],
        [
         let(Rel=att(relation)),
         translate([holds_in_relation_to,typeref],class(To))
        ]).
xmlpred(typeref,in_pm(M),phenotype_manifestation_type(M,Class),
        [
         let(Class=att(about))
        ]).



genid(Base,ID):-
        concat_atom(['_anon:',Base,'-'],Base1),
        gensym(Base1,ID).
      
        
/** <module>   
  @author Chris Mungall
  @version  $Revision: 1.6 $
  @date  $Date: 2005/10/24 01:42:24 $
  @license LGPL

  ---+ Name
%  pheno_xmlmap_phenoxml

  ---+ Synopsis

  ==
  :- use_module(bio(io)).
  :- use_module(bio(pheno_db)).
  :- use_module(bio(pheno_xmlmap_phenoxml)).

  demo:-
    load_biofile(phenoxml,'Rab1.phenoxml-xml'),
    setof(ID-Name,gene(ID,Name),GeneIDNames),
    writeln(genes=GeneIDNames).
  ==

  bridging layer from phenoxml-xml to native pheno model

  you should not need this module directly - handled by module io
  
  */
