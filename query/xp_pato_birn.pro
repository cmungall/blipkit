:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).

inh(atrophied,'Regional part of telencephalon').
inh(atrophied,'Regional part of cerebral white matter').
inh(atrophied,'Lobe parts of cerebral cortex').
inh(atrophied,'Regional part of cerebellar cortex').

inh(degenerate,'predominantly gray regional part of telencephalon').
inh(degenerate,'regional part of spinal cord').
inh(degenerate,'neuron').
inh(degenerate,'predominantly gray regional part of substantia nigra').

rq('has number of').
rq('has extra parts of type').
rq('has fewer parts of type').

tw('cell').
tw('neuron').
tw('astrocyte').
tw('cellular inclusion').
tw('molecule').
tw('protein').

rqi('regional part of neuron').
rqi('cellular component').
rqi('neuron').
rqi('cell').
rqi('predominantly gray regional part of telencephalon').
rqi('regional part of neostriatum').
rqi('regional part of telencephalon').
rqi('predominantly gray regional part of neostriatum').
rqi('regional part of parahippocampal gyrus').
rqi('regional part of basal part of pons').
rqi('regional part of diencephalon').
rqi('predominantly gray regional part of pontine tegmentum').
rqi('regional part of cerebral cortex').
rqi('regional part of cerebral peduncle').
rqi('predominantly gray regional part of substantia nigra').
rqi('lobe parts of cerebral cortex').
rqi('regional part of peripheral nervous system').
rqi('cytoarchitectural fields of hippocampal formation').
rqi('neuron process').

exclude(X,X).
exclude(cell,neuron).
exclude(cell,'regional part of neuron').
exclude(neuron,'regional part of neuron').
exclude(cell,'extramembrane region').
exclude(neuron,'extramembrane region').


generate_cdef(cdef(Q,[inheres_in=ES])):-
        inh(QN,EN),
        (   id_from_name(Q,QN)
        ;   id_from_name(Q,morphology)),
        id_from_name(E,EN),
        intra_ontology_subclassRT(E,ES).


generate_cdef(cdef(Q,[towards=E2,inheres_in=ES])):-
        rq(QN),
        rqi(EN),
        tw(E2N),
        \+ exclude(E2N,EN),
        id_from_name(Q,QN),
        id_from_name(E,EN),
        id_from_name(E2,E2N),
        intra_ontology_subclassRT(E,ES).


generate_cdef(cdef(Q,[towards=E2])):-
        rq(QN),
        tw(E2N),
        id_from_name(Q,QN),
        id_from_name(E2,E2N).

id_from_name(X,N) :-
        (   class(X,N),
            \+ sub_atom(X,_,_,_,'OBO-UBO')
        ->  true
        ;   class(X,N2),
            downcase_atom(N2,N),
            \+ sub_atom(X,_,_,_,'OBO-UBO')
        ->  true
        ;   throw(noclass(N))).

intra_ontology_subclassRT(X,Y) :-
        subclassRT(X,Y),
        belongs(X,O),
        belongs(Y,O).

