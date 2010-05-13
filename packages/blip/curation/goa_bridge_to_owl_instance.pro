/* -*- Mode: Prolog -*- */


:- module(goa_bridge_to_owl_instance,
          []).

:- use_module(bio(goa_db)).
:- use_module(bio(ontol_db)).
:- use_module(bio(seqfeature_db)).

:- discontiguous
        ontol_db:subclass/2,
        ontol_db:restirction/3.

% we treat each association as an class of a role

% treat the RoleID as the subset of gene products that exhibit properties in experiment
ontol_db:is_anonymous(RoleID):-
        association(RoleID,_,_,0).
ontol_db:subclass(RoleID,AgentID):-
        association(RoleID,_,AgentID,0).
ontol_db:class(RoleID,RoleName):-
        association(RoleID,_,AgentID,_),
        feature(AgentID,AgentName,_),
        concat_atom(['contextual subtype of ',AgentName],RoleName).

ontol_db:restriction(RoleID,Relation,ClassID):-
        association(RoleID,ClassID,_),
        agent_role_by_class(ClassID,Relation).
ontol_db:belongs(ID,association):-
        association(ID,_,_,_).

/** <module>
  @author Chris Mungall
  @version  $Revision: 1.1 $
  @date  $Date: 2005/12/03 00:06:23 $
  @license LGPL

  ---+ Name
  ---++ goa_bridge_to_class
- view go assocs as generic classs

  ---+ Synopsis

  ==
  :- use_module(bio(goa_bridge_to_owl_instance)).

  ==

  ---+ Description

  This maps the goa_db gene ontology association model to a generic
class model with a goa-OWL schema


 ---++ Command line usage

 exports a gene association file to OWL. Note that 'obo' can also be
used as an option
 
 ==
 blip -f go_assoc -i gene_assoc.fb.tbl -u goa_bridge_to_class -u goa_bridge_to_class io-convert -to owl -o assocs.owl
 ==

 ==
 blip -r so -r go -include "association ontology" -include "feature ontology" -f go_assoc -i lib/Test/data/gene_assoc.fb.tbl io-convert -u goa_bridge_to_class -u seqfeature_bridge_to_class -u ontol_db -to obo
 ==

  
**/