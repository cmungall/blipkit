/* -*- Mode: Prolog -*- */


:- module(omim_bridge_to_ontol,[]).
:- use_module(bio(aminoacid_chebi)).
:- use_module(bio(omim_db)).
:- use_module(bio(ontol_db)).
:- use_module(library(porter_stem)).

:- dynamic phenoclass/2.

prepend_prefix(ID,OboID):-
        atom_concat('MIM:',ID,OboID).

ontol_db:ontology(omim,omim,'Omim mapped to classes').

omim_class(OmimID,Class,N):-
        omim(OmimID,N),
        prepend_prefix(OmimID,Class).
omim_class(OmimID,Class):-
        omim_class(OmimID,Class,_).

ontol_db:class(Class):-
        omim_class(_,Class).
metadata_db:entity_label(Class,N):-
        omim_class(_,Class,N).
ontol_db:subclass(Class,'UBO:0000027'):-  % disease
        omim_class(_,Class).

ontol_db:class(Phenotype,Desc):-
        (   var(Phenotype)
        ->  omim_phenotype(_,_General,Desc),
            lookup_phenoclass(Desc,Phenotype)
        ;   phenoclass(Desc,Phenotype)).
ontol_db:subclass(Phenotype,'UBO:0000026'):-
        (   var(Phenotype)
        ->  omim_phenotype(_,_General,Desc),
            lookup_phenoclass(Desc,Phenotype)
        ;   phenoclass(_,Phenotype)).

metadata_db:entity_resource(Class,omim):-
        omim_class(_,Class).
metadata_db:entity_resource(Phenotype,omim):-
        (   var(Phenotype)
        ->  omim_phenotype(_,_General,Desc),
            lookup_phenoclass(Desc,Phenotype)
        ;   true).
metadata_db:entity_comment(Class,Comment):-
        omim_class(ID,Class),
        omim_prop(ID,'OMIM-tx',Comment).

metadata_db:entity_synonym(Class,Synonym):-
        omim(ID,_),
        omim_prop(ID,'OMIM-syn',Synonym),
        prepend_prefix(ID,Class).

ontol_db:restriction(Class,'OBD_OMIM:has_phenotype',Phenotype):-
        omim_phenotype(ID,_General,Desc),
        prepend_prefix(ID,Class),
        lookup_phenoclass(Desc,Phenotype).

% mutations are instances of omim diseases
ontol_db:inst(MutIDFull,Desc):-
        omim_mutation(MutID,_,Desc),
        prepend_prefix(MutID,MutIDFull).
ontol_db:inst_of(MutIDFull,OmimIDFull):-
        omim_mutation(MutID,OmimID,_),
        prepend_prefix(MutID,MutIDFull),
        prepend_prefix(OmimID,OmimIDFull).
ontol_db:inst_sv(MutIDFull,'rdfs:comment',Text,'xsd:text'):-
        omim_mutation_text(MutID,Text),
        prepend_prefix(MutID,MutIDFull).
        
ontol_db:inst(FxnID,''):-
        mut_fxn(_,FxnID).
ontol_db:inst_of(FxnID,'SO:0000109'):- % variant
        mut_fxn(_,FxnID).

ontol_db:inst_rel(MutIDFull,'OBD_OMIM:has_allele',MutFxnID):-
        mut_fxn(MutID,MutFxnID),
        prepend_prefix(MutID,MutIDFull).        
ontol_db:inst_rel_anon(MutFxnID,'OBD_OMIM:has_wildtype_aa',ChemID):-
        mut_fxn(MutID,MutFxnID),
        omim_mutation_fxn(MutID,_,_,AA,_),
        aminoacid_chebi(AA,ChemID,_).
ontol_db:inst_rel_anon(MutFxnID,'OBD_OMIM:has_mutant_aa',ChemID):-
        mut_fxn(MutID,MutFxnID),
        omim_mutation_fxn(MutID,_,_,_,AA),
        aminoacid_chebi(AA,ChemID,_).

ontol_db:inst_sv(MutFxnID,'OBD_OMIM:has_position',Pos,'xsd:number'):-
        omim_mutation_fxn(MutID,_,Pos,_,_),
        mut_fxn(MutID,MutFxnID).

mut_fxn(MutID,MutFxnID):-
        omim_mutation_fxn(MutID,_,Pos,_,_),
        prepend_prefix(MutID,MutIDFull),
        concat_atom([MutIDFull,Pos],'-',MutFxnID).

lookup_phenoclass(Desc,ID):-
        (   phenoclass(Desc,ID)
        ->  true
        ;   (   var(ID)
            ->  gensym('OMIM_Pheno:',ID),
                assert(phenoclass(Desc,ID))
            ;   throw(error(no_such_pheno_id(ID))))).
/** <module>   
  @author Chris Mungall
  @version  $Revision: 1.4 $
  @date  $Date: 2005/12/03 00:06:24 $
  @license LGPL

  ---+ Name
%  omim_bridge_to_ontol

  ---+ Synopsis
% 

  ==
  :- use_module(bio(io)).
  :- use_module(bio(omim_bridge_to_ontol)).
  :- use_module(bio(omim_db)).
  :- use_module(bio(ontol_db)).

  demo:-
      load_biofile(omimxml,'test2-omim.xml'),
      write_biofile(owl,'test2-omim.owl').

  ==

  ---+ Description

  Maps omim_db schema to ontological primitives

  The description below is in terms of an OWL mapping, although the
actual direct mapping is to the ontol_db schema, which has a direct
OWL translation
  
  ---++ OWL Mapping
  
encoding OMIM as RDFS/OWL

Should we encoded each OMIM entry as an instance or a class? Each OMIM
entry represents a disease or disease gene, which are types
instantiated by multiple disease or disease gene instances, so
owl:classes would be the appropriate representation. Actual disease
instances would be recorded in EHRs etc.

There are some practical problems in using classes - OMIM gives us no
hierarchy, so all classes would be in a flat space, which will look
pretty horrible if imported into an ontology editor/browser. At some
stage we may be able to infer a classification (either by mapping to
the new OBO disease ontology, or perhaps from the phenotypes).

Similarly, OMIM phenotypes also represent classes. We link between
them with a to-be-defined has_phenotype relation.

We treat the OMIM allele entries as instances of those OMIM
diseases. It could be argued that these should also be treated as
types - here we treat them as representative instances of the disease
in a representative individual who has a particular genotype. The
genotype is represented as a collection of has_allele links to
instances of SO sequence_variation. Each variation instance has a
has_wildtype_aa and a has_mutant_aa link to an anonymous/bnode
instance of a CHEBI amino acid class

  
  */
