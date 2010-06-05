%% Skolemizes introns using exon coords

%:- multifile intron_dnaseq_pos/5.
%:- multifile exon_intron_exon_transcript/4.

genome_db:intron(I) :-
        intron_transcript(I,_).

% TODO: deal with strands..
% todo: table_pred intron_transcript/2 doesn't work
% recommended: table_pred exon_transcript_order

genome_db:intron_transcript( intron(Seq,Beg,End,Str), T) :-
        exon_transcript_order(X1,T,R1),
        R2 is R1+1,
        exon_transcript_order(X2,T,R2),
        exon_dnaseq_pos(X1,Seq,_Beg,End,Str),
        exon_dnaseq_pos(X2,Seq,Beg,_End,Str).
        
genome_db:intron_dnaseq_pos(intron(Seq,Beg,End,Str),Seq,Beg,End,Str):-
        intron(intron(Seq,Beg,End,Str)).

genome_db:exon_intron_exon_transcript(X1,intron(Seq,End,Beg,Str),X2,T):-
        exon_transcript_order(X1,T,R1),
        exon_transcript_order(X2,T,R2),
        exon_dnaseq_pos(X1,Seq,_Beg,End,Str),
        exon_dnaseq_pos(X2,Seq,Beg,_End,Str),
        1 is R2-R1.        

genome_db:intron_transcript_order(intron(Seq,End,Beg,Str),T,R):-
        exon_dnaseq_pos(X1,Seq,_Beg,End,Str),
        exon_dnaseq_pos(X2,Seq,Beg,_End,Str),
        exon_transcript_order(X1,T,R),
        exon_transcript_order(X2,T,_).

seqfeature_db:featureloc(I,Seq,Beg,End,Str) :-
        I=intron(Seq,Beg,End,Str),
        intron(I).



equivalent_to(intron(Seq,Beg,End,Str),I) :-
        intron(I),
        intron_dnaseq_pos(I,Seq,Beg,End,Str).




