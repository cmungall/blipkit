/* -*- Mode: Prolog -*- */



:- module(pheno_db,
          [
           % intensional predicates
           genotype/2,
           genotype_phenotype/2,
           genotype_feature/2,
           genotype_organism/2,
           phenotype/2,
           phenotype_desc/2,
           phenotype_entity/2,
           entity_attribute/2,
           entity_class/2,
           attribute_measurement/4,
           attribute_class/2,
           attribute_stage_class/2,
           phenotype_pub/2,
           attribute_assay/2,
           
           % extensional predicates
           genetic_feature/3,
           entity_stage_class/2,
           phenotype_entity_class/2,
           phenotype_annot/5
          ]).
:- use_module(bio(ontol_db)).


% Data Predicates
%  
%  These predicates are purely extensional - they are expected to
%  come from an external data file

%% genotype(ID,Name)
%   an genotype can be mutant or wt, gene or variation feature
%  Intensional
%  
%% genotype_phenotype(ID,PID)
%   many to many
%  Intensional
%  
%% genotype_feature(ID,FeatureID)
%   genotype is mutant ot wt form of some feature, probably gene
%  Intensional
%
%% genotype_organism(?ID,?OrganismClassID)
%  
%  
%% phenotype(ID,Name)
%   optional label for a phenotype instance
%  Intensional
%  
%% phenotype_desc(ID,Desc)
%   optional full text description of phenotype
%  Intensional
%  
%% phenotype_entity(ID,EntityID)
%   the phenotype entity instance. 1 to many
%  Intensional
%  
%% entity_attribute(ID,AttributeID)
%   the attribute instance for a phenotype
%  Intensional
%
%% attribute_measurement(ID,UnitClassID,Value,Time)
%   measurement of some phenotypic attribute, eg cm 2
%
%% phenotype_pub(ID,PubID)
%   an ID for a publication describing phenotype
%  Intensional
%
%% attribute_assay(ID,AssayInstID)
%   an assay instance (may be a compound entity)
%  Intensional
%
%  

% metadata on the data predicates used in this module
:- use_module(bio(dbmeta)).
:- datapreds(pheno_db,
             [
              genotype(gid,n),
              genotype_phenotype(gid,pid),
              genotype_feature(gid,fid),
              genotype_organism(gid,oc),
              phenotype(pid,n),
              phenotype_desc(pid,d),
              phenotype_entity(pid,e),
              attribute_stage_class(a,sc),
              entity_attribute(e,a),
              entity_class(e,ec),
              attribute_measurement(a,uc,v,t),
              attribute_class(a,ac),
              phenotype_pub(pid,pubid),
              attribute_assay(a,assc)
             ]).

% --

% VIEW PREDICATES

genetic_feature(ID,N,T):-
        seqfeature_db:feature(ID,N,T).

%% phenotype_entity_class(?PID,?EC)
%   as per phenotype_entity/2 but unifies with class ID of entity
%  instance
%  
phenotype_entity_class(PID,EC):-
        phenotype_entity(PID,E),
        entity_class(E,EC).

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

  ---+ Conventions

  Entities and attributes are particular instances unless otherwise
denoted. Instances and classes are represented as class IDs, see class/2

  The suffix C denotes a class

  
  * E - entity; a particular instance of an entity (eg this fly wing)
  * EC - entity class; (eg ID for 'wing') see class/2
  * A - attribute; a particular instance of an attribute (eg this tail length)
  * AC - attribute class; (eg ID for 'wing') see class/2
  * PID - a particular phenotype (can encompass multiple entities)
  
  
**/