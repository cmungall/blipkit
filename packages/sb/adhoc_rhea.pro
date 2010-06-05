snapshot_diff(S,R,NumA,D) :-
        atom(NumA),
        !,
        atom_number(NumA,NumF),
        Num is round(NumF),
        snapshot_diff(S,R,Num,D).
snapshot_diff(S,R,1,R=X) :-
        !,
        snapshot_chebi(S,X).
snapshot_diff(S,R,Num,card(R,Num,Num)=X) :-
        Num>1,
        snapshot_chebi(S,X).

snapshot_chebi(S,X) :-
        snapshot_continuant(S,C),
        entity_xref(C,X),
        id_idspace(X,'CHEBI').

event_cdef(E,cdef('GO:0003824',Diffs)) :-
        findall(D,(event_input(E,I,Num),snapshot_diff(I,'OBO_REL:has_input',Num,D)),Ins),
        findall(D,(event_output(E,I,Num),snapshot_diff(I,'OBO_REL:has_output',Num,D)),Outs),
        setof(D,(member(D,Ins);member(D,Outs)),Diffs).

rhea2go_via(R,G,X) :-
        entity_xref(E,R),
        id_idspace(R,'Rhea'),
        entity_xref(E,X), % e.g. metacyc
        entity_xref(G,X),
        id_idspace(G,'GO').

expand_cdef(cdef(G,Diffs),cdef(G2,DiffsAllS)) :-
        class_cdef(G,cdef(G2,Diffs2)),
        append(Diffs,Diffs2,DiffsAll),
        setof(D,member(D,DiffsAll),DiffsAllS),
        !.
expand_cdef(X,X).


cdef_compare(D1,D2,Matches,X1s,X2s) :-
        expand_cdef(D1,D1X),
        expand_cdef(D2,D2X),
        D1X=cdef(G1,Diffs1),
        D2X=cdef(G2,Diffs2),
        findall(X,(member(R=X,Diffs1),\+member(R=X,Diffs2)),X1s),
        findall(X,(member(R=X,Diffs2),\+member(R=X,Diffs1)),X2s),
        (   G1=G2,X1s=[],X2s=[]
        ->  Matches=true
        ;   Matches=false).


% blip-findall -r goxp/molecular_function_xp_chebi -u adhoc_rhea -u pathway_db -r rhea -r go rhea2go_via/4
rhea2go_via(R,G,X,Match,X1s,X2s) :-
        rhea2go_via(R,G,X),
        entity_xref(E,R),
        event_cdef(E,RDef),
        class_cdef(G,GDef),
        cdef_compare(RDef,GDef,Match,X1s,X2s).



        
