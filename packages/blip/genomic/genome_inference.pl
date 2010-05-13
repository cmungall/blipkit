/* -*- Mode: Prolog -*- */

user:term_expansion((:- table(_)),
                    []) :- current_prolog_flag(dialect,swi).

:- table(intron/1).
:- table(exon/1).
:- table(a_intron/1).
:- table(a_exon/1).
:- table(s/2).
:- table(e/2).
:- table(p/3).
:- table(overlaps/2).
:- table(ends_after_start_of/2).
:- table(starts_before_end_of/2).

s(F,X):- p(F,X,_).
e(F,X):- p(F,_,X).
s(f(_,X,_),X) :- number(X).
e(f(_,_,X),X) :- number(X).


% internal exon from asserted intron
internal_exon(X) :- a_intron(I1),a_intron(I2),X=f(x,B,E),e(I1,B),s(I2,E),B<E, \+ ((a_intron(I3),overlaps(I3,X))).
%internal_exon2transcript(X,T) :- a_intron(I1),transcribed_in(I1,T),a_intron(I2),transcribed_in(I2,T),X=f(x,B,E),e(I1,B),s(I2,E),B<E, \+ ((a_intron(I3),overlaps(I3,X))).

intron(I) :- a_exon(X1),a_exon(X2),e(X1,B),s(X2,E),B<E,I=f(i,B,E), \+ ((a_exon(X3),overlaps(X3,I))).

first_exon(X) :- exon(X), \+ ((exon(X2),starts_before_start_of(X2,X))).
last_exon(X) :- exon(X), \+ ((exon(X2),ends_after_end_of(X2,X))).

overlaps(A,B):- ends_after_start_of(A,B),starts_before_end_of(A,B).

ends_after_start_of(A,B):- e(A,EA),s(B,SB),EA > SB.
ends_after_end_of(A,B):- e(A,EA),e(B,EB),EA > EB.
starts_before_end_of(A,B):- s(A,SA),e(B,EB),SA < EB.
starts_before_start_of(A,B):- s(A,SA),s(B,SB),SA < SB.


% test data
a_exon(x1).
transcribed_in(x1,t1).
p(x1,10,20).
a_exon(x2).
transcribed_in(x2,t1).
p(x2,30,40).
a_exon(x3).
p(x3,50,60).
transcribed_in(x3,t1).

exon(X):- a_exon(X).
intron(X):- a_intron(X).

a_intron(i1).
a_intron(i2).
p(i1,100,200).
p(i2,300,400).


%a_intron(fake).



