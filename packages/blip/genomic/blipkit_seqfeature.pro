/* -*- Mode: Prolog -*- */


:- module(blipkit_seqfeature,[]).

:- use_module(bio(blipkit)).
:- use_module(bio(seqfeature_db)).
:- use_module(bio(ontol_db)).
:- use_module(bio(dbmeta)).

:- blip('seqfeature-merge',
        'Merge features that have identical localizations',
        [atom([to,t],ToFormat),
         atom([o,output],OutFile)],
        FileL,
        (   
          load_factfiles(FileL),
          merge_facts(seqfeature_db:feature,feature_equivalent),
          write_biofile(ToFormat,OutFile))).

/** <module>   
  @author Chris Mungall
  @version  $Revision: 1.1 $
  @date  $Date: 2006/03/25 01:57:15 $
  @license LGPL

  ---+ Name
  ---++ blipkit
- simple interface to blip module functionality

  ---+ Description

  this is a submodule of blipkit for handling seqfeature_db*/