
% starts intron
genome_db:five_prime_cis_splice_site(splice_site(T,Seq,Beg,End,Str)) :-
        exon_dnaseq_pos(X,Seq,_,Beg,Str),
        exon_transcript_order(X,T,R),
        downstream_n(Beg,2,End,Str),
        %\+ final_exon_on_transcript(X,T).
        \+ ((exon_transcript_order(_,T,R2),
             R2>R)).

% finishes intron
genome_db:three_prime_cis_splice_site(splice_site(T,Seq,Beg,End,Str)) :-
        exon_dnaseq_pos(X,Seq,End,_,Str),
        exon_transcript_order(X,T,R),
        R>0, % not the first
        upstream_n(End,2,Beg,Str).

% TODO: intermediate types
genome_db:splice_site(X) :- five_prime_cis_splice_site(X).
genome_db:splice_site(X) :- three_prime_cis_splice_site(X).

% TODO: move
downstream_n(P,N,P2,Str) :-
        P2 is P + N*Str.
upstream_n(P,N,P2,Str) :-
        P2 is P - N*Str.



% TODO: splice junction
