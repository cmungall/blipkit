:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).

qn(quality).
qn(duration).
qn(arrested).
%qn(irregular).
qn(rate).
qn('decreased rate').
qn('increased rate').
qn(disrupted).
qn(abolished).

pn('cellular carbohydrate metabolic process').
pn('amino acid metabolic process').

prokp(P):-
        entity_partition(P,gosubset_prok),
        subclassRT(P,R),
        pn(RN),
        class(R,RN).

generate_cdef(cdef(Q,[inheres_in=E])):-
        qn(QN),
        class(Q,QN),
        prokp(E).
