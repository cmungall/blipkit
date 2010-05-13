

genotypes([G|GL]) --> genotype(G),sep,!,genotypes(GL).
genotype(A1/A2) --> allele(A1),slash,!,allele(A2).
genotype(A) --> allele(A). % allowed?
allele(A-Q) --> uallele(A),openb,!,qual(Q),closeb.
allele(A) --> uallele(A).
uallele(wt) --> "wt".
uallele(unknown) --> "unknown".
uallele(deletion(P)) --> "del",pos(P).
uallele(triplication(P)) --> "trip",pos(P).
pos(X) --> a(X).

sep --> ws(*),";",!,ws(*).
slash --> ws(*),"/",!,ws(*).

ws(*) --> " ",!,ws(*).
ws(*) --> [].






