/* -*- Mode: Prolog -*- */



:- module(seqfeature_bridge_from_fasta,
          []).

seqfeature_db:feature_residues(ID,Seq):- fasta_db:fastaseq(ID,_,Seq).
seqfeature_db:featureprop(ID,description,Desc):-
        fasta_db:fastaseq(ID,Desc,_).

/** <module>
  @author Chris Mungall
  @version  $Revision: 1.1 $
  @date  $Date: 2005/10/14 20:16:57 $
  @license LGPL

  ---+ Name
  ---++ seqfeature_bridge_from_fasta
- view layer

  ---+ Synopsis

  ==
  :- use_module(bio(seqfeature_bridge_from_fasta)).

  ==

  ---+ Description

  feature_residues/2 is a view over fastaseq/3 sequences

  featureprop/3 is a view over fastaseq/3 with type 'description'
  
**/