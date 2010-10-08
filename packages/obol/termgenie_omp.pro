:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).
:- use_module(bio(tabling)).
:- use_module(bio(index_util)).
:- use_module(bio(io)).
:- use_module(quickterm).

:- initialization
        absolute_file_name(blipkit('obol/conf/mireot_omp-ontol_db.pro'),F),
        load_biofile(ontol_db:pro,F),
        user:optimize_generate_cdef.

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

generate_all :-
        generate(E,Q,Msg),
        writeln(E/Q/Msg),
        fail.

generate(E,Q,Msg) :-
        rate_quality(QN),
        class(Q,QN),
        metabolic_process(E),
        template_request(omp_entity_quality(E,Q),
                         Msg,
                         [suppress_reasoner(true),
                          ontology_dir(.),
                          idnum_min(1),
                          commit(true)
                          ]).





