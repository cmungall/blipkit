:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).
:- use_module(bio(tabling)).
:- use_module(bio(index_util)).
:- use_module(bio(io)).
:- use_module(classdef_parser).

:- multifile term_label/3.
:- multifile term_textdef/3.

:- initialization load_biofile(ontol_db:pro,'conf/mireot_omp-ontol_db.pro'),user:optimize_generate_cdef.

user:optimize_generate_cdef :-
        table_pred(ontol_db:subclassRT/2),
        materialize_index(user:metabolic_process(1)).
        
%:- initialization table_pred(ontol_db:subclassRT/2).
%:- initialization materialize_index(user:metabolic_process(1)).

rate_quality('increased rate').
rate_quality('decreased rate').

metabolic_process(P) :-
        %subclassRT(P,'GO:0008152'),
        subclass(P,'GO:0008152'),
        entity_partition(P,gosubset_prok).

user:generate_cdef(Q that inheres_in(P)) :-
        rate_quality(QN),
        class(Q,QN),
        metabolic_process(P).

term_label(Q that inheres_in(P)) --> quality(Q),[of],process(P).

quality(Q) --> terminal(Q).
process(P) --> terminal(P).

term_textdef(Q that inheres_in(P)) --> ['Having a'],quality(Q),[of],process(P).




