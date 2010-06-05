/* -*- Mode: Prolog -*- */

:- module(phylo_bridge_from_taxon,[]).
:- use_module(bio(taxon_db)).
:- use_module(bio(metadata_db)).
:- use_module(bio(phylo_db)).

% phylonode/1 mapped to taxname/2
phylo_db:phylonode(ID):-
        taxname(ID,_).
metadata_db:entity_label(ID,Name):-
        taxname(ID,Name).
phylo_db:phylonode_parent(ID,PID,null(_)):-
        taxparent(ID,PID).


/** <module>   
  @author Chris Mungall
  @version  $Revision: 1.1 $
  @date  $Date: 2005/12/03 00:06:24 $
  @license LGPL

  ---+ Name
%  phylo_bridge_from_taxon

  ---+ Synopsis
% 

  ==
  :- use_module(bio(io)).
  :- use_module(bio(phylo_bridge_from_taxon)).
  :- use_module(bio(taxon_db)).
  :- use_module(bio(phylo_db)).

  ==

  ---+ Description
  
  mapping from the taxon_db module to the phylo_db module. phylo predicates
can be used to view taxon data

  ---++ Mapping

  
  * taxname/2 and taxparent/2 to phylonode/4
  
  
  */
