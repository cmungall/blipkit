:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).
:- use_module(classdef_parser).

phalanx_quality(Q) :- morpho_quality(Q).
phalanx_quality(duplicated).

epiphysis_quality(Q) :- morpho_quality(Q).

epiphysis_relational_quality('lacks all physical parts of type').

morpho_quality('decreased size').
morpho_quality('increased size').
morpho_quality('hypoplastic').
morpho_quality('curved').
morpho_quality('size').
morpho_quality('quality').
morpho_quality('morphology').
morpho_quality('conical').
morpho_quality('ivory').
morpho_quality('triangular').

lateral(X) :- differentium(X,has_coordinate,_).
lateral(X) :- differentium(X,_,Y),lateral(Y).

phalanx('UBERON:0003221').
phalanx(E) :-
        class(R,'Phalanx of toe'),
        subclassRT(E,R),
        \+ lateral(E).
phalanx(E) :-
        class(R,'Phalanx of finger'),
        subclassRT(E,R),
        \+ lateral(E).

% use epiphysis 
epiphysis(E) :- class(E,'Epiphysis').
% FMA has no further subtypes


user:generate_cdef(Q that inheres_in(E) and inheres_in_part_of(P)) :-
        epiphysis_quality(QN),
        class(Q,QN),
        phalanx(P),
        epiphysis(E).

user:generate_cdef(Q that inheres_in(E) and towards(D)) :-
        epiphysis_relational_quality(QN),
        class(Q,QN),
        phalanx(P),
        epiphysis(D).


