/* -*- Mode: Prolog -*- */


:- module(ontol_bridge_inst_to_class,
          [
          ]).

:- use_module(bio(ontol_db),[inst/2,inst_of/2,inst_rel/3,inst_sv/4,belongs/2]).

ontol_db:class(C,N):- inst(C,N).
ontol_db:subclass(C,P):- inst_of(C,P).
ontol_db:restriction(C,R,To):- inst_rel(C,R,To).
ontol_db:class_comment(C,Comment):-
        inst_sv(C,'http://www.w3.org/2000/01/rdf-schema#comment',Comment,_).
ontol_db:belongs(C,O):-
        inst_of(C,P),
        C\=P,
        belongs(P,O).

/** <module>   
  @author Chris Mungall
  @version  $Revision: 1.10 $
  @date  $Date: 2005/09/30 16:23:45 $
  @license LGPL

  ---+ Name
%  ontol_bridge_inst_to_class

  ---+ Synopsis

  ==
  ==

  ---+ Description

  rectifies a not-atypical mistake, namely modeling universals as instances and not classes

  NOTE: this converts all instances to classes without exception

  NOTE: does not interact well with ontol_bridge_from_owl. Try this:
  
  ==
  blip io-convert -i foo.owl -to obo > foo.obo
  blip io-convert -i foo.obo -to obo -u ontol_bridge_inst_to_class > foo-i2c.obo
  blip io-convert -i foo-i2c.obo -to obo -u ontol_bridge_ignore_inst > foo-c.obo
  
  ==
  
  */