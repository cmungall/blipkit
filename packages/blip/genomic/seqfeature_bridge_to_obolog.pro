/* -*- Mode: Prolog -*- */

:- module(seqfeature_bridge_to_obolog,[]).
:- use_module(bio(seqfeature_db)).
:- use_module(bio(obolog_db)).

obolog_db:formula(instance_of(F,T)) :- feature_type(F,T).
obolog_db:formula(has_start_i(F,J)) :- feature_start_junction(F,J).
obolog_db:formula(has_end_i(F,J)) :- feature_end_junction(F,J).



/** <module>   

  ---+ Synopsis

  ==
  :- use_module(bio(seqfeature_bridge_to_obolog)).
  ==

  ---+ Description

  bridging layer from seqfeature_db model to formula/1 facts in obolog_db

  you should not need this module directly - handled by module io

  ---++ Mapping

  * feature_type/3 is mapped to (instance_of ?f ?t)
  * feature_start_junction/2 and feature_end_junction/2 are mapped to  (has_start ?f ?p) and (has_end ?f ?p)
  ** skolem functions are used to generate identifiers for junctions

  ---++ Command line usage

  Generates a KIF/CL syntax file:
  
 ==
 blip -f gff3 -i volvox_all.gff3 -u seqfeature_bridge_to_obolog io-convert -to obolog
 ==

  Generates a prolog obolog_db, consisting of reified axioms (formula/1):
  
 ==
 blip -f gff3 -i volvox_all.gff3 -u seqfeature_bridge_to_obolog io-convert -to obolog
 ==

  Unreified axioms, with types shifted out the domain of discourse:
  
 ==
 blip -f gff3 -i volvox_all.gff3 -u seqfeature_bridge_to_obolog io-convert -to obolog_prolog 
 ==

  This will write a prolog program representing the GFF. Each feature is represented using a unary fact
  
  */