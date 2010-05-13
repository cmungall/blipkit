:- module(hmm,
          [most_probable_hmm_path/2]).

% taken from http://www.ling.gu.se/~lager/Spaghetti/spaghetti.html

% hardcode output probabilities
% todo: this should come directly from an external source
outprob(a,det,0.300).
outprob(can,aux,0.010).
outprob(can,v,0.005).
outprob(can,n,0.001).
outprob(he,pron,0.070).

% hardcode transition probabilities
% todo: this should come directly from an external source
transprob(start,det,0.30).          transprob(v,det,0.36).
transprob(start,aux,0.20).          transprob(v,aux,0.01).
transprob(start,v,0.10).            transprob(v,v,0.01).
transprob(start,n,0.10).            transprob(v,n,0.26).
transprob(start,pron,0.30).         transprob(v,pron,0.36).
transprob(det,det,0.20).            transprob(n,det,0.01).
transprob(det,aux,0.01).            transprob(n,aux,0.25).
transprob(det,v,0.01).              transprob(n,v,0.39).
transprob(det,n,0.77).              transprob(n,n,0.34).
transprob(det,pron,0.01).           transprob(n,pron,0.01).
transprob(aux,det,0.18).            transprob(pron,det,0.01).
transprob(aux,aux,0.10).            transprob(pron,aux,0.45).
transprob(aux,v,0.50).              transprob(pron,v,0.52).
transprob(aux,n,0.01).              transprob(pron,n,0.01).
transprob(aux,pron,0.21).           transprob(pron,pron,0.01).


%% most_probable_hmm_path(+Words,?Path) is det
% true if Path is a list of the most probable output types for each token in Word
most_probable_hmm_path(Words,Path) :-
        probable_paths(Words,[1-[start]],PPaths),
        keymax(PPaths,_P-Path1),
        reverse(Path1,[start|Path]).

probable_paths([],PPaths,PPaths).
probable_paths([Word|Words],PPaths0,PPaths) :-
        findall(PPath,
                (   outprob(Word,Tag2,PL),
                    findall(P2-[Tag2,Tag1|Tags],
                            (   member(P1-[Tag1|Tags],PPaths0),
                                transprob(Tag1,Tag2,PT), 
                                P2 is PL*PT*P1),
                            AllPaths),
                    keymax(AllPaths,PPath)),
                PPaths1),
        probable_paths(Words,PPaths1,PPaths).

keymax(AllPaths, MaxProb-PPath) :-
        keymax(AllPaths, -1, [start], MaxProb, PPath).
 
%----------------------------------------------------+
% keymax(+Elements,+AkkuNum,+AkkuPath,-Max,-MaxPath)
%----------------------------------------------------+
% stop: keine elemente mehr
keymax([], Max, MaxPath, Max, MaxPath).
% nimmt das bisher groesste mit
keymax([Num1-Val1|Tail], Num2, Val2, Max, MaxPath) :-
        (   Num1 > Num2
        ->  keymax(Tail, Num1, Val1, Max, MaxPath)
        ;   keymax(Tail, Num2, Val2, Max, MaxPath)
        ).             

% -------------------- TESTS --------------------
% regression tests to ensure behaviour of module is correct;
% lines below here are not required for module functionality

unittest(test(foo,
            [],
            (   
                most_probable_hmm_path([he,can,can,a,can],Path),
                writeln(Path)),
            Path=[pron,aux,v,det,n])).

