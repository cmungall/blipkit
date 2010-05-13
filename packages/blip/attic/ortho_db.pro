/* -*- Mode: Prolog -*- */



:- module(ortho_db,
          [
           % extensional
           ortholog/5,

           % intensional
           ortholog/3,
           ortholog/4,
           ortholog_feature/2
           ]).


% metadata on the data predicates used in this module
:- use_module(bio(dbmeta)).
:- datapreds(ortho_db,
             [
              ortholog(id,fid1,fid2,type,score)
             ]).

%% ortholog(?ID,?F1,?F2)
%   as ortholog/5
%  
ortholog(ID,F1,F2):-
        ortholog(ID,F1,F2,_,_).
%% ortholog(?ID,?F1,?F2,?Type)
%   as ortholog/5
%  
ortholog(ID,F1,F2,T):-
        ortholog(ID,F1,F2,T,_).
        
%% ortholog_feature(?ID,F?)
%   symmetric
%  
ortholog_feature(ID,F):-
        ortholog(ID,F,_).
ortholog_feature(ID,F):-
        ortholog(ID,_,F).
/** <module>
  @author Chris Mungall
  @version  $Revision: 1.1 $
  @date  $Date: 2005/11/18 22:09:48 $
  @license LGPL

  ---+ Name
  ---++ ortho_db
- schema for orthology data

  ---+ Synopsis

  ==
  :- use_module(bio(ortho_db)).

  ==

  ---+ Description

**/