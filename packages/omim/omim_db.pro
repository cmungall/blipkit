/* -*- Mode: Prolog -*- */


:- module(omim_db,
          [
           % extensional predicates
           omim/2,
           omim_prop/3,
           omim_phenotype/3,
           omim_mutation/3,
           omim_mutation_text/2,
           omim_mutation_fxn/5
           % intensional predicates
          ]).

:- use_module(bio(dbmeta)).
:- use_module(bio(bioprolog_util)).
:- use_module(bio(mode)).

% metadata on the data predicates used in this module
:- datapreds(omim_db,
             [
              omim(pk('OmimID'),
                   atom('Name')),
              omim_prop(fk('OmimID',omim),
                        atom('Type'),
                        atom('Value')),
              omim_phenotype(fk('OmimID',omim),
                             atom('Class'),
                             atom('Description')),
              omim_mutation(pk('MutID'),
                            fk('OmimID',omim),
                            atom('Desc')),

              omim_mutation_text(fk('MutID',omim_mutation),
                                 atom('Text')),
              
              omim_mutation_fxn(fk('MutID',omim_mutation),
                                atom('Gene'),
                                atom('Codon'),
                                atom('AminoAcidWild'),
                                atom('AminoAcidMutant'))
              
             ]).

% --

% VIEW PREDICATES

/** <module>   
  @author Chris Mungall
  @version  $Revision: 1.12 $
  @date  $Date: 2005/12/03 00:06:24 $
  @license LGPL

  ---+ Name
  ---++ omim_db
- omim disease schema

  ---+ Synopsis
  
  ==
  :- use_module(bio(io)).
  :- use_module(bio(omim_db)).

  ==
  
  ---+ Description

  a simple schema that mirrors a partially normalised OMIM

  
  */