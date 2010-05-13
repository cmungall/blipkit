
% TODO: strandedness
genome_db:exon_transcript_order(X,T,R) :-
        %transcript(T),
        % Seq is left unbound
        setof(B-X1,E^Str^(exon_transcript(X1,T),exon_dnaseq_pos(X1,_Seq,B,E,Str)),PosExonPairs), % TODO
        nth0(R,PosExonPairs,_-X).



