/* -*- Mode: Prolog -*- */

:- module(pheno_db,
          [
           % intensional predicates
           genotype/1,
           genotype/2,
           genotype_feature/2,
           genotype_organism/2,
           phenotype/1,
           phenotype_manifestation/3,
           phenotype_manifestation_desc/2,
           phenotype_manifestation_provenance/2,
           phenotype_manifestation_provider/2,
           phenotype_manifestation_type/2,
           phenotype_class/2,
           phenotype_phenochar/2,
           phenochar_desc/2,
           phenochar_bearer_class/2,
           phenochar_quality/2,
           phenochar_quality_class/2,
           quality_class/2,
           %quality_measurement/4,
           quality_qualifier/3,
           quality_related_entity/2,
           
           % extensional predicates
           genotype_phenotype/2,
           genotype_phenochar/2,
           genetic_feature/3,
           entity_stage_class/2,
           phenotype_entity_class/2,
           phenotype_annot/5,
           quality_composed_type/2,
           phenotype_annotation_entity_ontology/2
           
          ]).

:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).
:- use_module(bio(dbmeta)).

%%  genotype(?Genotype) is nondet
%    
%    true if Genotype identifies a genotype
:- extensional(genotype/1).
%TODO: fix persistent db

%%  genotype(?Genotype,?Name) is nondet
:- multifile genotype/2.
genotype(G,N):- entity_label(G,N),genotype(G).

%%  genotype_feature(?Genotype,?Feature) is nondet
%    
%    A feature such as a gene variant defining a genotype
:- extensional(genotype_feature/2).

%%  genotype_organism(?Genotype,?OrganismClass) is nondet
%    
%    
:- extensional(genotype_organism/2).

%%  phenotype(?Phenotype) is nondet
%    
%    A collection of characteristics  
:- extensional(phenotype/1).

%%  phenotype_manifestation(?Manifestation,?Phenotype,?Genotype) is nondet
%    
%    An instantiation of a phenotype in one or more individuals
:- extensional(phenotype_manifestation/3).

%%  phenotype_manifestation_provenance(?Manifestation,?PubID) is nondet
%    
%    provenance entity
%:- extensional(phenotype_manifestation_provenance/2).
phenotype_manifestation_provenance(M,Pub):-
        entity_source(M,Pub),
        phenotype_manifestation(M,_,_).
        
%%  phenotype_manifestation_provider(?Manifestation,?Provider) is nondet
%    
%    agent/organisation that created the annotation
:- extensional(phenotype_manifestation_provider/2).

%%  phenotype_manifestation_desc(?Manifestation,?Description) is nondet
%    
%    A description of a particular phenotype instance
:- extensional(phenotype_manifestation_desc/2).

%%  phenotype_manifestation_type(?Manifestation,?Class) is nondet
%    
%    eg genetic context
:- extensional(phenotype_manifestation_type/2).

%%  phenotype_class(?Phenotype,?Class) is nondet
%    
%    A pre-coordinated class defining the phenotype. Note that most classes may be post-coordinated
:- extensional(phenotype_class/2).

%%  phenotype_phenochar(?Phenotype,?Character) is nondet
%    
%    A link between phenotype and a EQ pair
:- extensional(phenotype_phenochar/2).

%%  phenochar_desc(?Character,?Desc) is nondet
%    
%    Description of EQ pair
:- extensional(phenochar_desc/2).

%%  phenochar_bearer_class(?Character,?Class) is nondet
%    
%    The Entity type
:- extensional(phenochar_bearer_class/2).

%%  phenochar_quality(?Character,?Quality) is nondet
%    
%    Link to a quality
:- extensional(phenochar_quality/2).

%%  quality_class(?Quality,?Class) is nondet
%    
%    Quality type - eg from PaTO
:- extensional(quality_class/2).

%% phenochar_quality_class(?PC,?QC) is nondet
phenochar_quality_class(PC,QC):-
        phenochar_quality(PC,Q),
        quality_class(Q,QC).

%%  quality_related_entity(?Quality,?Class) is nondet
%    
%    Additional entity - eg for relational qualities
:- extensional(quality_related_entity/2).

%%  quality_qualifier(?Quality,?Relation,?To) is nondet
%    
%    Further refines quality
:- extensional(quality_qualifier/3).

% --

% VIEW PREDICATES


genotype_phenotype(Gt,P):-
        phenotype_manifestation(_,P,Gt).

genotype_phenochar(Gt,PC):-
        phenotype_manifestation(_,P,Gt),phenotype_phenochar(P,PC).

genetic_feature(ID,N,T):-
        seqfeature_db:feature(ID,N,T).

%% phenotype_entity_class(?PID,?EC)
%   as per phenotype_entity/2 but unifies with class ID of entity
%  instance
%  
phenotype_entity_class(P,EC):-
        phenotype_phenochar(P,PC),
        phenochar_bearer_class(PC,EC).

%% entity_attribute_class(?PID,?AC)
%   as per entity_attribute/2 but unifies with class of attribute
%  instance
%  
entity_attribute_class(PID,AC):-
        entity_attribute(PID,A),
        attribute_class(A,AC).

%% phenotype_annot(?PID,?GID,?EC,?AC,?SC)
%   combines phenotype_entity/2 with entity_class/2
%  cartesian product, but should be 1:1
%  
phenotype_annot(PID,G,EC,AC,SC):-
        phenotype_entity(PID,E),
        genotype_phenotype(G,PID),
        entity_class(E,EC),
        entity_attribute(E,A),
        attribute_class(A,AC),
        (   entity_stage_class(E,SC)
        ->  true
        ;   SC=null).

%% attribute_measurement(?AID,?UnitClassID,?Value)
%   as attribute_measurement/4
%  
attribute_measurement(A,U,V):-
        attribute_measurement(A,U,V,_).

%% entity_stage_class(?E,?SC)
%   view over entity_attribute/2 and entity_stage_class/2 - an entity can have attributes at many stages so this is 1:many
%  
entity_stage_class(E,SC):-
        entity_attribute(E,A),
        attribute_stage_class(A,SC).

quality_composed_type(Q,Genus^['OBO_REL:inheres_in'=BearerClass|Diffs]):-
        quality_class(Q,Genus),
        phenochar_quality(PC,Q),
        phenochar_bearer_class(PC,BearerClass),
        findall(towards=E2,quality_related_entity(Q,E2),Diffs1),
        findall(R=To,quality_qualifier(Q,R,To),Diffs2),
        append(Diffs1,Diffs2,Diffs).


%% attribute_assay_class(?PID,?AssayC)
%   as per attribute_assay/2 but returns class of instance
%
%  the instance may be a particular observation and may have
%attribute-value measurements associated with it. the assay class is
%the class of assay, taken from some assay ontology
%  
attribute_assay_class(PID,AssayC):-
        attribute_assay(PID,Assay),
        inst_of(Assay,AssayC).

phenotype_annotation_entity_ontology(M,O):-
        phenotype_manifestation(M,P,_),
        phenotype_entity_class(P,PC),
        belongs(PC,O).


% -------------------- STATS --------------------
:- multifile dbmeta:schema_statistic/2.

dbmeta:schema_statistic(pheno_db,count(phenotype_annotations,Num)):-
        setof_count(X,phenotype_manifestation(X,_,_),Num).
dbmeta:schema_statistic(pheno_db,count(annotated_genotypes,Num)):-
        setof_count(X,phenotype_manifestation(_,_,X),Num).
dbmeta:schema_statistic(pheno_db,count(distinct_phenotypes,Num)):-
        setof_count(X,quality_composed_type(_,X),Num).
dbmeta:schema_statistic(pheno_db,count_by(phenotype_annotations_by_ont,Num)):-
        count_by(X,phenotype_annotation_entity_ontology(_,X),Num).
dbmeta:schema_statistic(pheno_db,count_by(phenotype_annotations_by_provider,Num)):-
        count_by(X,phenotype_manifestation_provider(_,X),Num).


/** <module>
  @author Chris Mungall
  @version  $Revision: 1.5 $
  @date  $Date: 2005/11/18 22:02:51 $
  @license LGPL

  ---+ Name
  ---++ pheno_db
- datalog schema for phenotypes and genotypes

  ---+ Synopsis

  ==
  :- use_module(bio(pheno_db)).

  ==

  ---+ Description

**/
