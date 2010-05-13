:- [query_obo].
:- use_module(bio(ontol_writer_obo)).

wa:-
        forall((   label_match_xp(_A,B,CDef,_),
                   CDef=cdef(G,Diffs),
                   debug(hp,'cdef=~w',[CDef]),
                   maplist(repl,Diffs,Diffs2)),
               write_cdef(obo,B,cdef(G,Diffs2))).


repl(R=X,R=Y):-
        atom_concat('MA:',_,X),
        mapmouse(X,Y),
        !.
repl(_=X,_):-
        atom_concat('MA:',_,X),
        !,
        class(X,XN),
        format(standard_error,'Cannot find: ~w ~w',[X,XN]),
        fail.
repl(D,D).


mapmouse(X,Y):-
        class_xref(U,X),
        class_xref(U,Y),
        belongs(Y,'fma').

label_match_xp(A,B,CDef,E):-
        belongs(A,'MPheno.ontology'),
        class(A,N),
        class(B,N),
        belongs(B,'medical_genetics'),
        A\=B,
        class_cdef(A,CDef),
        \+ class_cdef(B,_),
        CDef=cdef(_,Diffs),
        member('OBO_REL:inheres_in'=E,Diffs).



