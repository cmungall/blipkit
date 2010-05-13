/* -*- Mode: Prolog -*- */

/** <module> (very incomplete) wrapper to Sean Eddy's SQUID library.
  
  ==
  :- use_module(bio(squid)).
  test:-
    seqh_new(Sh,'my.fasta'),
    seqh_iterate(Sh,Seq,writeln(seq=Seq)).
  ==
  
  ---+ Setup

  You need libsquid to use this module
  http://selab.wustl.edu/cgi-bin/selab.pl?mode=software#squid

  First install libsquid, then run

  ==
  configure
  ./make
  ==

  in this directory

  Not much here yet - just revcomp and seqfile parsing. this is just
proof of concept, should be easy to extend to get any squid
functionality
  
  */

:- module(squid,
          [
           revcomp/2,
           seqh_new/2,
           seqh_close/1,
           seqh_read/2,
           seqh_iterate/3
          ]
         ).

:- initialization load_foreign_library(squid4pl).

quicktest :-
        revcomp('TCAAAG',R),
        writeln(R),
        seqh_new(SH,'z.fa'),
        seqh_read(SH,Seq),
        writeln(Seq),
        seqh_close(SH),
        rf('z.fa').

rf(F):-
        seqh_new(SH,F),
        seqh_iterate(SH,Seq,writeln(seq=Seq)).

%% revcomp(+Seq,?RevSeq)

%% seqh_new(?SeqH,+FileName)

%% seqh_close(+SeqH)

%% seqh_read(+SeqH,?Seq)

%% seqh_iterate(+Sh,?Seq,+Goal)
seqh_iterate(Sh,Seq,Code):-
        repeat,
        (seqh_read(Sh,Seq)
        ->  Goal,
            fail
        ;   !),
        seqh_close(Sh).
