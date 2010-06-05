/* -*- Mode: Prolog -*- */


:- module(go,
          [read_assoctable/2,
           lookup_assoc_by_termacc/3,
           lookup_assoc_by_termacc/4,
           termacc_prodacc/3
           ]).

:- use_module(library(table)).
:- use_module(bio(ontol_db),
              [parentRT/2]).

read_assoctable(F,H):-
        new_table(F,
                  [
                   proddb(atom),
                   prodacc(atom),
                   prodsymbol(atom),
                   qualifier(atom),
                   termacc(atom),
                   ref(atom),
                   evcode(atom),
                   with(atom),
                   aspect(atom),
                   prodname(atom),
                   prodsyn(atom),
                   prodtype(atom),
                   prodtaxa(atom),
                   assocdate(atom)
                   ],
                  [field_separator(9)
                  ],
                  H).

lookup_assoc_by_termacc(H,ID,Spec):-
        lookup_assoc_by_termacc(H,ID,_,Spec).
lookup_assoc_by_termacc(H,ID,IDc,Spec):-
        setof(IDc,parentRT(IDc,ID),IDcL),
        in_table(H,[termacc(IDc)|Spec],_),
        %parentRT(IDc,ID).
        member(IDc,IDcL).

termacc_prodacc(H,TID,PID):-
        in_table(H,[termacc(TID),prodacc(PID)],_).
/** <module>   
  @author Chris Mungall
  @version  $Revision: 1.5 $
  @date  $Date: 2005/06/22 02:10:32 $
  @license LGPL


  @synopsis

  :- use_module(bio(io_go_assoc)).

  @detail

  input/output module for GO association files

  use SWI library(table) for fast reading of tab-delim files

  TODO: deal with GFF split ranges
  
  */
