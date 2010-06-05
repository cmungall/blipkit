/* -*- Mode: Prolog -*- */

:- module(seqfeature_owlmap_gdc,[]).
:- use_module(bio(seqfeature_db)).
:- use_module(library('thea2/owl2_model')).
:- [library('thea2/hooks/owl2_export_rdf_safe')].
:- [seqfeature_owlmap_shared]. % junction_uri/2

owl2_model:namedIndividual(F) :- feature(F).
owl2_model:classAssertion(T,F) :- feature_type(F,T).
owl2_model:propertyAssertion(starts_on_i,F,J) :- feature_start_junction(F,JX),junction_uri(JX,J).
owl2_model:propertyAssertion(ends_on_i,F,J) :- feature_end_junction(F,JX),junction_uri(JX,J).

owl2_model:namedIndividual(J) :- junction(JX),junction_uri(JX,J).
owl2_model:classAssertion(junction,J) :- junction(JX),junction_uri(JX,J). % TODO: map

% j(Seq,Pos,Strand)
owl2_model:propertyAssertion(number_of_upstream_bases,J,literal(type(integer,Pos))) :- junction(JX),junction_uri(JX,J),JX=j(_,Pos,_).
%owl2_model:propertyAssertion(number_of_upstream_bases,JX,literal(Pos)) :- junction(JX),junction_uri(JX,j(_,Pos,_)).

owl2_model:propertyAssertion(in_sequence,J,SeqStr) :- junction(JX),junction_uri(JX,J),JX=j(Seq,_,Str),atom_concat(Seq,Str,SeqStr).



     

/** <module> Maps seqfeature_db model to OWL2 treating features as molecular entities

  ---+ Synopsis

  ==
  :- use_module(bio(seqfeature_owlmap_molecular)).
  ==

  ---+ Description

  bridging layer from seqfeature_db model to owl2 via thea

  you should not need this module directly - handled by module io

  ---++ Mapping

  * feature_type/3 is mapped to classAssertion/2
  * feature_start_junction/2 and feature_end_junction/2 are mapped to propertyAssertion using has_start_i and has_end_i objectProperties

  ---++ Command line usage
  
 ==
 blip -f gff3 -i volvox_all.gff3 -u seqfeature_bridge_to_owl2 io-convert -to owl2
 ==

  
  */
