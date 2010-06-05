/* -*- Mode: Prolog -*- */

:- use_module(bio(seqfeature_db)).
:- use_module(bio(ontol_db),[class/2,property/2]).

:- discontiguous
        ontol_db:inst_of/2,
        ontol_db:inst_sv/3.

% convert a name to an ID. SO should be loaded
n2cid(N,ID):-
        (   class(ID,N)
        ->  true
        ;   property(ID,N)
        ->  true
        ;   ID=N).

% construct a unique ID from a prolog term
makeid(Term,ID):-
        Term =.. TL,
        concat_atom([orphan|TL],'::',ID).

feature_oboid(F,F):-
        atom(F),!.
feature_oboid(F,ID):-
        F =.. TL,
        concat_atom(TL,'__',ID).

% (+,?) d DEPREC??
feature_id_safe(ID,ID2):-
        feature_dbxref(ID,0,ID2),
        !.
feature_id_safe(ID,ID2):-
        atom_concat([orphan,ID],':',ID2).

%ontol_db:inst_rel(ID,T,PID):-
%        feature_relationship(ID,PID,T1),
%        n2cid(T1,T).

ontol_db:inst_sv(ID,S,V,'xsd:string'):-
        featureprop(ID,S,V).
ontol_db:inst_sv(ID,sox:has_residues,V,'xsd:string'):-
        feature_residues(ID,V).
ontol_db:inst_sv(ID,sox:has_seqlen,V,'xsd:int'):-
        feature_seqlen(ID,V).

metadata_db:entity_xref(ID,X):-
        feature_dbxref(ID,X).
