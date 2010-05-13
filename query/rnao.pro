:- use_module(library(lists)).

:- table motif/1.

motif(X):- stemloop(X).

:- table pseudoknot/1.
pseudoknot(X):-
        stemloop(SL1),
        stemloop(SL2),
        has_part(SL2,S2),
        stem(S2),
        has_part(SL1,L1),
        loop(L1),
        part_of(S2,L1),
        mereological_union(SL1,SL2,X).

:- table stemloop/1.
stemloop(X):-
        stem(S),
        loop(L),
        end_of_strand1(S,B1),
        has_start_base(L,B1),
        start_of_strand2(S,B2),
        has_end_base(L,B2),
        mereological_union(S,L,X).

:- table tetraloop/1.
tetraloop(X):-
        loop(X),
        X=[A,B,C,D],
        five_prime_to(S1,A),
        five_prime_to(D,S2),
        paired(S1,S2).

:- table gnra_tetraloop/1.
gnra_tetraloop(X):-
        tetraloop(X),
        has_base_at(X,B1,1),
        has_base_at(X,B3,3),
        has_base_at(X,B4,4),
        g(B1),
        purine(B3),
        a(B4).

:- table gnra_tetraloop_with_ths/1.
gnra_tetraloop_with_ths(X):-
        gnra_tetraloop(X),
        has_base_at(X,B1,1),
        has_base_at(X,B4,4),
        g(B1),
        purine(B3),
        a(B4).

:- table bulge/1.
bulge(L):-
        loop(L),
        stem(S1),
        end_of_strand1(S1,S1E1),
        has_start_base(L,S1E1),
        has_end_base(L,S2S1),
        stem(S2),
        start_of_strand1(S2,S1E1),
        end_of_strand2(S2,S2J),
        start_of_strand1(S1,S2J).

% connection between two strands
connects_5_3(X,Y):-
        has_end_base(X,B1),
        has_start_base(Y,B2),
        five_prime_to(X,Y).
% connection between stem and strand
connects_5_3(X,Y):-
        end_of_strand1(X,B1),
        has_start_base(Y,B2),
        five_prime_to(X,Y).

:- table internal_loop/1.
internal_loop(L):-
        stem(S1),
        stem(S2),
        loop_chain(LC1),
        loop_chain(LC2),
        connects_5_3(S1,LC1),
        connects_5_3(LC1,S2),
        connects_5_3(S2,LC2),
        connects_5_3(LC2,S1).

:- table internal_loopset/1.
internal_loopset(L):-
        stem(S1),
        loop(L1),
        stem(S2),
        loop(L2),
        connects_5_3(S1,L1),
        connects_5_3(L1,S2),
        connects_5_3(S2,L2),
        connects_5_3(L2,S1),
        mereological_union(L1,L2,L).

loop_chain([L]):- loop(L).

:- table base_pair/1.
base_pair(BP):-
        base(X),
        base(Y),
        paired(X,Y),
        mereological_union(X,Y,BP).

:- table consecutive_bases/1.
consecutive_bases(BP):-
        base(X),
        base(Y),
        five_prime_to(X,Y),
        mereological_union(X,Y,BP).

:- table nt_sequence/1.
nt_sequence([S]):-
        base(S).

nt_sequence([X,Y|T]):-
        five_prime_to(X,Y),
        nt_sequence([Y|T]).

:- table loop/1.
loop(S):-
        nt_sequence(S).

%% stem(X)
% stored as two seqs, the second is 3 to 5
:- table stem/1.
stem([X-Y]):-
        paired(X,Y).
stem([X-Y,X2-Y2|L]):-
        paired(X,Y),
        five_prime_to(X,X2),
        five_prime_to(Y2,Y),
        stem([X2-Y2|L]).

base(X):- purine(X).
base(X):- pyrimidine(X).

purine(X):- a(X).
purine(X):- u(X).
pyrimidine(X):- g(X).
pyrimidine(X):- c(X).


:- table has_base_at/3.
has_base_at(X,B,Pos):-
        nth(Pos,X,B).
        
:- table has_start_base/2.
has_start_base([X|_],X).

:- table has_end_base/2.
has_end_base(S,X):-
        reverse(S,[X|_]).

:- table start_of_strand1/2.
start_of_strand1([X-_|_],X).

:- table end_of_strand1/2.
end_of_strand1(SS,X):-
        reverse(SS,[X-_|_]).

:- table start_of_strand2/2.
start_of_strand2(SS,X):-
        reverse(SS,[_-X|_]).

:- table end_of_strand1/2.
end_of_strand2([_-X|_],X).

% sub-relations
:- table paired/2.
paired(X,Y):- paired_cww(X,Y).
paired(X,Y):- paired_ths(X,Y).
paired(X,Y):- paired(Y,X).

%has_strand(SS,S):-
%        strand

mereological_union(X,Y,Z):-
        append(X,Y,Z).

five_prime_to(b1,b2).
five_prime_to(b2,b3).
five_prime_to(b3,b4).
five_prime_to(b4,b5).
five_prime_to(b5,b6).
five_prime_to(b6,b7).
five_prime_to(b7,b8).
paired_cww(b1,b8).
paired_cww(b2,b7).
paired_ths(b3,b6).

a(b1).

c(b2).
g(b3).
a(b4).
a(b5).
a(b6).
g(b7).

u(b7).


%base(X):- five_prime_to(X,_).
%base(X):- five_prime_to(_,X).
