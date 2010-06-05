/* -*- Mode: Prolog -*- */


% NOTES: had to jump through some hoops here to avoid infinite loops
% deriving facts based on existing derived facts isnt such a good idea
% in future, consider doing one-off rather than dynamic
% i.e. infer everything in one slurp, then assert
% can keep dynamic by using a big dynamic pred

:- module(chado_bridge_from_game,
          []).

:- use_module(bio(seqfeature_db)).
:- use_module(bio(bioseq)).
:- use_module(bio(range)).

start_codon_pp(C,P):-
        seqfeature_db:feature(C,_,start_codon),
        feature_relationship(P,C,coded_by).

start_codon_pp_seq(C,P,PSeq,PR):-
        feature(C,_,start_codon),
        feature_relationship(P,C,coded_by),
        debug(foo,'pp ~w ~w',[P,C]),
        pp_from_start_codon(C,PSeq,PR).
        
seqfeature_db:feature(P,P,polypeptide):-
        feature(C,_,start_codon),
        feature_relationship(P,C,coded_by).
seqfeature_db:feature_residues(T,TSeq):-
        feature(T,_,_),
        concat_subfeature_residues(T,exon,TSeq).
seqfeature_db:feature_residues(PID,PSeq):-
        start_codon_pp_seq(_,PID,PSeq,_).
seqfeature_db:feature_relationship(P,T,derives_from):-
        feature(C,_,start_codon),
        feature(T,_,mRNA),
        feature_relationship(C,T,part_of),
        feature_relationship(P,C,coded_by),
        debug(foo,'fr5 ~w ~w ~w',[P,T,C]).
seqfeature_db:featureloc(P,S,Beg,End,Dir,0,0,[]):-
        feature(C,_,start_codon),
        feature_relationship(P,C,coded_by),
        debug(foo,'floc ~w',[P]),
        start_codon_pp_seq(_,P,_,range(S,Beg,End,Dir)).
%seqfeature_db:featureloc(T,S,Beg,End,Dir,0,0,[]):-
%        feature(T,_,mRNA),
%        feature_relationship(P,C,coded_by),
%        debug(foo,'floc ~w',[P]),
%        start_codon_pp_seq(_,P,_,range(S,Beg,End,Dir)).
seqfeature_db:feature_organism(F,Org):-
        featureloc(F,S,_,_,_),
        feature_organism(S,Org).
/** <module>
  @author Chris Mungall
  @version  $Revision: 1.2 $
  @date  $Date: 2005/12/07 00:06:41 $
  @license LGPL

  ---+ Name
  ---++ chado_bridge_from_game
- maps game seqfeature semantics to chado semantics

  ---+ Synopsis

  ==
  :- use_module(bio(chado_bridge_from_game)).

  ==

  ---+ Description

**/