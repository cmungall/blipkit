
%% single_exon_gene(G)
% see SO:
genome_db:single_exon_gene(G):-
        exon_gene(X,G),
        \+ ((exon_gene(X2,G),
             \+ X2=X)).
%single_exon_gene(G):-
%        1 is count(X,exon_gene(X,G)).
