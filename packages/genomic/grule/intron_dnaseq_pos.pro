
genome_db:intron_dnaseq_pos(I,Seq,Beg,End,Str) :-
        intron(I),I\=intron(_,_,_,_),feature_dnaseq_pos(I,Seq,Beg,End,Str).


