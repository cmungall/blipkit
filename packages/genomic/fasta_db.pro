/* -*- Mode: Prolog -*- */


:- module(fasta_db,
          [
           fastaseq/3
           ]).

:- use_module(bio(dbmeta)).

%% fastaseq(?ID,?Header,?Seq)
% @param ID identifier, should be unique but not guaranteed by fasta format
% @param Header description, possibly structured
% @param Seq atom of sequence in IUPAC chars
:- extensional(fastaseq/3).

/** <module> prolog schema for fasta format

  ---+ Synopsis

  ==
  :- use_module(bio(fasta_db)).
  :- use_module(bio(io)).

  demo:-
    load_biofile(fasta,'utr.fasta'),
    forall( (fastaseq(ID,_,Seq),sub_atom(Seq,Pos,_,_,'CGCATATC')),
            format('Subseq in ~w at ~w~n',[ID,Pos])).
  ==

  ---+ Description

  fasta sequence model

  sequence data can also be modeled in the seqfeature_db module - see
feature_residues/2

  however, the seqfeature_db module may be overkill for some
applications where it is desirable to have a simple fasta style
ID-header-sequence model

  in addition, the seqfeature_db model requires a feature type for
each feature, which is not stored in a standard way in the fasta model
  
**/
