/* -*- Mode: Prolog -*- */


:- module(seqfeature_bridge_to_inst,[
                                    ]).
:- use_module(bio(seqfeature_db)).
:- use_module(bio(ontol_db),[]).

:- [seqfeature_bridge_to_ontol_shared].

:- discontiguous
        ontol_db:inst_of/2,
        ontol_db:inst_sv/3.

ontol_db:inst(F) :-        feature(F).
ontol_db:inst(J) :-        junction(JX),junction_id(JX,J).
ontol_db:inst_of(F,T) :- feature_type(F,T).
ontol_db:inst_of(J,'SO:0000699') :- junction(JX),junction_id(JX,J).

ontol_db:inst_rel(F,T,PF):-
        feature_relationship(F,PF,T1),
        n2cid(T1,T).

ontol_db:inst_rel(F,'SO:has_start',J):- feature_start_junction(F,JX),junction_id(JX,J).
ontol_db:inst_rel(F,'SO:has_end',J):- feature_end_junction(F,JX),junction_id(JX,J).

ontol_db:inst_rel(J,'SO:part_of',Seq) :- junction(JX),JX=j(Seq,_P,_Str),junction_id(JX,J).
ontol_db:inst_sv(J,'SO:pos',P,'xsd:integer') :- junction(JX),JX=j(_,P,_),junction_id(JX,J).

% j(Seq,Pos,Strand)
junction_id(J,ID) :- J=..L,concat_atom(L,'__',ID).




/** <module> maps features to instances
  
  ---+ Description

  bridging layer from extensional data predicates in seqfeature module
to inst/2, inst_sv/3 predicates in the ontol module

  you should not need this module directly - handled by module io

  ---++ Command line usage

  exports a gene association file to OWL.
 
 ==
 blip -f chaos -i Rab1.chaos-xml -u seqfeature_bridge_to_inst io-convert -to owl -o Rab1.owl
 ==

  Same to OBO
  
 ==
 blip -f chaos -i Rab1.chaos-xml -u seqfeature_bridge_to_inst io-convert -to obo -o Rab1.obo
 ==
  
  */
