/* -*- Mode: Prolog -*- */

:- module(seqfeature_owlmap_molecular,[]).
:- use_module(bio(seqfeature_db)).
:- use_module(bio(owl2_model)).
:- [seqfeature_owlmap_shared].


owl2_model:class(F) :- feature(F).
owl2_model:subClassOf(T,F) :- feature_type(F,T).
owl2_model:subClassOf(F,someValuesFrom(has_start,J)) :- feature_start_junction(F,JX),junction_uri(JX,J).
owl2_model:subClassOf(F,someValuesFrom(has_end,JX)) :- feature_end_junction(F,JX),junction_uri(JX,J).
owl2_model:subClassOf(F,allValuesFrom(has_start,J)) :- feature_start_junction(F,JX),junction_uri(JX,J).
owl2_model:subClassOf(F,allValuesFrom(has_end,JX)) :- feature_end_junction(F,JX),junction_uri(JX,J).

% enumerate bases and junctions
%base(

% j(Seq,Pos,Strand)
junction_uri(J,ID) :- J=..L,concat_atom(L,'__',ID).
     

/** <module> Maps seqfeature_db model to OWL2 treating features as molecular entities

  ---+ Synopsis

  ==
  :- use_module(bio(seqfeature_bridge_to_owl2)).
  ==

  ---+ Description

  bridging layer from seqfeature_db model to owl2 via thea.

  This mapping treats features as molecular entities (independent
  continuants). This means that every gene feature maps to a
  class. Instances would be the actual molecule region instances and
  would not be explicitly represented.

  Contrast with seqfeature_owlmap_gdc.pro

  ---++ Mapping

  * feature_type/3 is mapped to subClassOf/2
  * feature_start_junction/2 and feature_end_junction/2 are mapped to both existential and universal restrictions

  ---++ Command line usage
  
 ==
 blip -f gff3 -i volvox_all.gff3 -u seqfeature_owlmap_molecular io-convert -to owl2
 ==

  
  */
