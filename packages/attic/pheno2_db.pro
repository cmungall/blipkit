/* -*- Mode: Prolog -*- */



:- module(pheno2_db,
          [
           % intensional predicates
           genotype/2,
           genotype_feature/2,
           genotype_organism/2,
           phenotype/2,
           phenotype_manifestation/3,
           phenotype_manifestation_desc/2,
           phenotype_class/2,
           phenotype_phenochar/2,
           phenochar_desc/2,
           phenochar_bearer_class/2,
           phenochar_quality/2,
           quality_class/2,
           %quality_measurement/4,
           quality_qualifier/3,
           
           % extensional predicates
           genotype_phenotype/2,
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

% metadata on the data predicates used in this module
:- use_module(bio(dbmeta)).
:- datapreds(pheno2_db,
             [
              genotype(pk('Genotype'),
                       atom('Name'))
             - 'A collection of features defining the genetic complement of an organisn',

              genotype_feature(pk('Genotype'),
                               fk('Feature',feature))
             - 'A feature such as a gene variant defining a genotype',

              genotype_organism(pk('Genotype'),
                                fk('OrganismClass',class))
             - '',

              phenotype(pk('Phenotype'),
                        atom('Name'))
             - 'A collection of general characteristics',

              phenotype_manifestation(pk('Manifestation'),
                                      fk('Phenotype',phenotype),
                                      fk('Genotype',genotype))
             - 'An instantiation of a phenotype in one or more individuals',

              phenotype_manifestation_desc(fk('Manifestation',phenotype_manifestation),
                                           atom('Description'))
             - 'A description of a particular phenotype instance',

              phenotype_class(fk('Phenotype',phenotype),
                              fk('Class',class))
             - 'A pre-coordinated class defining the phenotype. Note that most classes may be post-coordinated',

              phenotype_phenochar(fk('Phenotype',phenotype),
                                  fk('Character',phenochar))
             - 'A link between phenotype and a EQ pair',

              phenochar_desc(fk('Character',phenochar),
                             atom('Desc'))
             - 'Description of EQ pair',

              phenochar_bearer_class(fk('Character',phenochar),
                                     fk('Class',class))
             - 'The Entity type',

              phenochar_quality(fk('Character',phenochar),
                                fk('Quality',quality))
             - 'Link to a quality',

              quality_class(fk('Quality',quality),
                            fk('Class',class))
             - 'Quality type - eg from PaTO',

              quality_qualifier(fk('Quality',quality),
                                fk('Relation',property),
                                fk('To',class))
             - 'Further refines quality'
             ]).

% --

% VIEW PREDICATES

genotype_phenotype(Gt,P):-
        phenotype_manifestation(_,P,Gt).

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