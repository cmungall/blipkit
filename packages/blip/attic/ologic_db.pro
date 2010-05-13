/* -*- Mode: Prolog -*- */



:- module(ologic,
          []).

% metadata on the data predicates used in this module
:- use_module(bio(dbmeta)).
:- datalog_schema
        universal(pk('U'))
        - 'true if U is the name of a universal',

        relation(pk('R'))
        - 'true if R is the name of a relation',

        % do as universal relation?
        isa(fk('U',universal),
            fk('U2',universal))
        - 'true if all Us are also U2s at all times',

        universal_relation(fk('R',relation),
                           fk('U',universal),
                           fk('U2',universal))
        - 'true if all Us R some U2',

        % make these FKs to general entity?
        instance(pk('I'))
        - 'true if I is the name of an instance',

        instantiates(fk('I',instance),
                     fk('U',instance),
                     fk('T',chronoid))
        - 'true if I instantiated U at T. Time variant relation'.
        
        chronoid(pk('T'))
        - 'true if T is the name of a time point or interval',

        instance_relation(fk('R',relation),
                          fk('I',instance),
                          fk('I2',instance))
        - 'true if I stands in R to I2. Time invariant relation',

        instance_relation(fk('R',relation),
                          fk('I',instance),
                          fk('I2',instance),
                          fk('T',chronoid))
        - 'true if I stands in R to I2 at T. Time variant relation'.

has_participant(P,C):-
        universal_relation(P,i(has_participant),C).
part_of(P,C):-
        universal_relation(P,i(part_of),C).
/** <module>
  @author Chris Mungall
  @version  $Revision: 1.1 $
  @date  $Date: 2006/03/25 01:57:15 $
  @license LGPL

  ---+ Name
  ---++ ologic
- 

  ---+ Synopsis

  ==

  ==

  ---+ Description

  DEPREC - see obd_db
  
**/