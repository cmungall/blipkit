
% note that this does not uniquify the codon
genome_db:start_codon(codon(start,P)):- cds(P).
genome_db:stop_codon(codon(stop,P)):- cds(P).

genome_db:start_codon_exon_pos(codon(start,P),X,Beg):-
        cds_exon_pos_pair(P,X,Beg,_,_).
genome_db:stop_codon_exon_pos(codon(stop,P),X,End):-
        cds_exon_pos_pair(P,_,_,X,End).
