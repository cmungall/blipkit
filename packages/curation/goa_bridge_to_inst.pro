/* -*- Mode: Prolog -*- */


:- module(goa_bridge_to_inst,
          []).

:- use_module(bio(goa_db)).
:- use_module(bio(ontol_db)).

:- discontiguous
        ontol_db:inst_of/2,
        ontol_db:inst_rel/3.

% we treat each association as an instance of a role
ontol_db:inst_of(RoleID,ClassID):-
        association(RoleID,ClassID,_).

ontol_db:inst_rel(F,R,Role):-
        association(Role,Class,F),
        agent_role_by_class(Class,R).        

        
/** <module>
  @author Chris Mungall
  @version  $Revision: 1.1 $
  @date  $Date: 2005/12/03 00:06:23 $
  @license LGPL

  ---+ Name
  ---++ goa_bridge_to_inst
- view go assocs as generic instances

  ---+ Synopsis

  ==

  ==

  ---+ Description

  This maps the goa_db gene ontology association model to a generic
instance model with a goa-OWL schema

  With the goa model, loose "associations" are recorded between IDs
for classes in an ontology and feature IDs. This mapping assumes that
each association is an instance of a role played by the feature. We
use the has_role relation.

 In future it may be better to use specific relations - eg
has_location between a feature and a cellular component

 ---++ Command line usage

 exports a gene association file to OWL. Note that 'obo' can also be
used as an option
 
 ==
 blip -f go_assoc -i gene_assoc.fb.tbl -u goa_bridge_to_inst -u goa_bridge_to_inst io-convert -to owl -o assocs.owl
 ==

**/