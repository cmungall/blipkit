:- module(lego_db,
          [
           action/1,
           process/1,
           activates/2,
           inhibits/2,
           part_of/2,
           occurs_in/2,
           enabled_by/2
           ]).

:- use_module(bio(dbmeta)).
:- use_module(bio(metadata_db)).
:- use_module(bio(bioprolog_util),[call_unique/1,solutions/3]).

:- extensional(action/1).
:- extensional(process/1).
:- extensional(activates/2).
:- extensional(inhibits/2).
:- extensional(part_of/2).
:- extensional(occurs_in/2).
:- extensional(enabled_by/2).

/** <module> represents biological legos

  ---+ Synopsis

==
:- use_module(bio(lego_db)).

% 
demo:-
  nl.
  

==

---+ Details

See exploring_legos.txt

---+ Additional Information



*/
