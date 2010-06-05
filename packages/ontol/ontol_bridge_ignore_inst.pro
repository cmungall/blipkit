/* -*- Mode: Prolog -*- */


:- module(ontol_bridge_ignore_inst,
          [
          ]).

:- use_module(bio(ontol_db),[]).

:- retractall(ontol_db:inst(_,_)).
:- retractall(ontol_db:inst_of(_,_)).

/** <module>   
  @author Chris Mungall
  @version  $Revision: 1.10 $
  @date  $Date: 2005/09/30 16:23:45 $
  @license LGPL

  ---+ Name
%  ontol_bridge_ignore_inst

  ---+ Synopsis

  ==
  ==

  ---+ Description

  removes inst level statements

  TODO!!!!! DOES NOT WORK
  
  NOTE: does not interact well with ontol_bridge_from_owl. Try this:
  
  ==
  blip io-convert -i foo.owl -to obo > foo.obo
  blip io-convert -i foo.obo -to obo -u ontol_bridge_inst_to_class > foo-i2c.obo
  blip io-convert -i foo-i2c.obo -to obo -u ontol_bridge_ignore_inst > foo-c.obo
  
  ==
  
  */