/* -*- Mode: Prolog -*- */


:- module(go,
          [read_assoctable/2,
           lookup_assoc_by_termacc/3,
           lookup_assoc_by_termacc/4
           ]).

:- use_module(library(table)).
:- use_module(bioprolog(ontol_db),
              [parentRT/2]).

%% read_assoctable(+Filename,?Handle)
%   opens a table for reading: must be tab delimited file in
%  standard go assoc format
%  
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

%% lookup_assoc_by_termacc(+Handle,+TermID,?Spec) nd
%% lookup_assoc_by_termacc(+Handle,+TermID,?DirectTermID,?Spec) nd
%   does a transitive search by ID and finds all associations
%  attached to that term or children of that term
%
%  See the swi library(table) docs for details of Spec. Columns are:
%
%                     proddb(atom),
%                   prodacc(atom),
%                   prodsymbol(atom),
%                   qualifier(atom),
%                   termacc(atom),
%                   ref(atom),
%                   evcode(atom),
%                   with(atom),
%                   aspect(atom),
%                   prodname(atom),
%                   prodsyn(atom),
%                   prodtype(atom),
%                   prodtaxa(atom),
%                   assocdate(atom)
%
%  
lookup_assoc_by_termacc(H,ID,Spec):-
        lookup_assoc_by_termacc(H,ID,_,Spec).
lookup_assoc_by_termacc(H,ID,IDc,Spec):-
        setof(IDc,parentRT(IDc,ID),IDcL),
        in_table(H,[termacc(IDc)|Spec],_),
        %parentRT(IDc,ID).
        member(IDc,IDcL).
/** <module>   
  @author Chris Mungall
  @version  $Revision: 1.5 $
  @date  $Date: 2005/06/22 02:10:32 $
  @license LGPL


  ==
  :- use_module(bio(io_go_assoc)).
  q(TermID):-
      read_assoctable('fly.gene_association',H),
      Spec=[prodacc(ProdAcc),prodsymbol(ProdSym),evcode('IDA')],
      lookup_assoc_by_termacc(H,TermID,DirectTermID,Spec),
      format('Product ~w ~w assigned directly to ~w~n',
             [ProdAcc,ProdSym,DirectTermID]).
  ==
  
  ---+

  input/output module for GO association files

  use SWI library(table) for fast reading of tab-delim files
  
  */