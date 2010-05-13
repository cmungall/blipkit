/* -*- Mode: Prolog -*- */


:- module(seqfeature_bridge_to_ontol_nary_inst,[
                                    ]).
:- use_module(bio(seqfeature_db)).
:- use_module(bio(ontol_db)).

:- [seqfeature_bridge_to_ontol_shared].

:- discontiguous
        ontol_db:inst_of/2,
        ontol_db:inst_sv/3.

ontol_db:inst_of(F,T):-
        feature_type(F,TN),
        n2cid(TN,T).

ontol_db:inst(ID):-
        feature(ID).

ontol_db:inst_rel(ID,sox:has_start,Beg,Seq):-
        featureloc(ID,Seq,Beg,_,_).

ontol_db:inst_rel(ID,sox:has_end,End,Seq):-
        featureloc(ID,Seq,_,End,_).

ontol_db:inst_rel(ID,sox:has_strand,Strand,Seq):-
        featureloc(ID,Seq,_,_,Strand).


/** <module> manifests seqfeature_db as reified ontology instances using 3-ary relations


  ---+ Synopsis

  ==
  :- use_module(bio(seqfeature_bridge_to_ontol_nary_inst)).

  ==

  ---+ Description


  bridging layer from extensional data predicates in seqfeature module
to inst/2, inst_sv/3 predicates in the ontol module

  you should not need this module directly - handled by module io

  ---++ Command line usage

  exports a gene association file to OBO. Note that OWL will not work due to n-ary relations
 
 ==
 blip -f chaos -i Rab1.chaos-xml -u seqfeature_bridge_to_ontol_nary_inst io-convert -to obo -o Rab1.obo
 ==

  
  
  */