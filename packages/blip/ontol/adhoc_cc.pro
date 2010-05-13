:- use_module(bio(ontol_db)).
:- use_module(bio(ontol_writer_obo)).


fix_lumen(Lumen) :-
        class(MEL,'membrane-enclosed lumen'),
        class(M,membrane),
        class_cdef(Lumen,cdef(MEL,[surrounded_by=O])),
        differentium(OM,surrounds,O),
        class_cdef(OM,cdef(M,[surrounds=O])),
        write_cdef(obo,Lumen,cdef(MEL,[bounded_by=OM])),
        fail.

double_membrane(DM) :-
        class(Membrane,membrane),
        subclassT(DM,Membrane),
        subclass(DM_1,DM),
        subclassT(DM_1,Membrane),
        subclass(DM_2,DM),
        DM_1\=DM_2,
        subclassT(DM_1,Membrane),
        true.

        
        
