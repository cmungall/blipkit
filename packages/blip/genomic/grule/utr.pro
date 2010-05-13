
%three_prime_UTR(U) :- exon_dnaseq_pos(X,Seq,B,E,Str),exon_transcript(Seq,T),cds_transcript(CDS,T),

three_prime_UTR(utr(T,Seq,Beg,End,Str)) :- exon_transcript_rank(X,T,0),exon_dnaseq_pos(X,Seq,Beg,_,Str),cds_transcript(CDS,T),cds_dnaseq_pos(CDS,Seq,End,_,Str).
five_prime_UTR(utr(T,Seq,Beg,End,Str)) :- exon_transcript_rank(X,T,0),exon_dnaseq_pos(X,Seq,_,End,Str),cds_transcript(CDS,T),cds_dnaseq_pos(CDS,Seq,_,Beg,Str).

utr(X) :- three_prime_UTR(X).
utr(X) :- five_prime_UTR(X).

genome_db:utr_dnaseq_pos(utr(_,Seq,Beg,End,Str),Seq,Beg,End,Str):-
        utr(utr(Seq,Beg,End,Str)).



