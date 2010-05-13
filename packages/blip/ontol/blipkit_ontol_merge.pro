/* -*- Mode: Prolog -*- */


:- module(blipkit_ontol_merge,[]).


:- use_module(bio(bioprolog_util)).
:- use_module(bio(ontol_db)).
:- use_module(bio(io)).

:- blip('ontol-merge',
        'Merges classes from ontologies based on identical names',
        [atom([to,t],ToFormat),
         atom([o,output],OutFile)],
        _,
        (   ensure_loaded(bio(dbmeta)),
            merge_facts(ontol_db:class,
                        blipkit_ontol_merge:class_pair_by_same_name(_,_)),
            write_biofile(ToFormat,OutFile))).

class_pair_by_same_name(X,Y):-
        class(X,N),
        class(Y,N),
        X\=Y.

/** <module>   
  @author Chris Mungall
  @version  $Revision: 1.29 $
  @date  $Date: 2006/02/21 01:52:17 $
  @license LGPL

  ---+ Name
  ---++ blipkit
- simple interface to blip module functionality

  ---+ Description

  this is a submodule of blipkit for handling ontology related data*/