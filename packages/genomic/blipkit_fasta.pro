/* -*- Mode: Prolog -*- */


:- module(blipkit_fasta,[]).

:- use_module(bio(blipkit)).
:- use_module(bio(fasta_db)).
:- use_module(bio(bioseq)).
:- use_module(bio(io)).

:- blip('fasta-subseqs',
        'probes all fastaseqs with seqs in query file',
        [atom([queryfile,qf],QF),
	 number([window,win],Window,0),
         bool([reverse,rev],Rev)],
        _,
        (
          ensure_loaded(bio(fasta_db)),
          load_biofile(user:fasta,QF),
          forall(user:fastaseq(ID,_,Seq),
                 (   show_subseq_positions(ID,'+',Seq,Window),
		     (   Rev=1
                     ->  revcomp(Seq,RCSeq),
                         show_subseq_positions(ID,'-',RCSeq,Window)
                     ;   true))))).

show_subseq_positions(SubID,Ori,SubSeq,0):-
	!,
	debug(fasta,'searching for ~w on ~w',[SubID,Ori]),
        forall( (   fastaseq(ID,_,Seq),
                    sub_atom(Seq,Pos,_,_,SubSeq)),
                format('~w ~w ~w ~w~n',[SubID,Pos,ID,Ori])).

show_subseq_positions(SubID,Ori,SubSeqFull,W):-
	!,
	debug(fasta,'searching for ~w (window=~w) on ~w',[SubID,W,Ori]),
	sub_atom(SubSeqFull,0,W,_,SubSeq), % truncate to window size
	atom_length(SubSeqFull,Len),
        forall( (   fastaseq(ID,_,Seq),
                    sub_atom(Seq,Pos,W,_,SubSeq),
                    sub_atom(Seq,Pos,Len,_,CurSubSeq),
		    extend_match(W,Len,CurSubSeq,SubSeqFull,MatchLen) ),
                format('~w ~w ~w ~w ~w~n',[SubID,Pos,MatchLen,ID,Ori])).

extend_match(W,W,_,_,W):- !.
extend_match(W,Len,S1,S2,ML):-
	sub_atom(S1,W,1,_,Sub),
	sub_atom(S2,W,1,_,Sub),
	!,
	W2 is W+1,
	extend_match(W2,Len,S1,S2,ML).
extend_match(W,_,_,_,W):- !.

/** <module>   
  @author Chris Mungall
  @version  $Revision: 1.5 $
  @date  $Date: 2005/11/23 20:09:16 $
  @license LGPL

  ---+ Name
%  blipkit_fasta

  ---+ Description

  this is a submodule of blipkit for handling fasta_db*/