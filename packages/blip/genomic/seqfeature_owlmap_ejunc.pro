:- use_module(seqfeature_db).
:- use_module(seqfeature_owlmap_gdc).

:- [seqfeature_owlmap_shared]. % junction_uri/2


:- multifile seqfeature_db:junction/1.
seqfeature_db:junction(X) :- enumerated_junction(X).

owl2_model:propertyAssertion(immediately_before_i,J1,J2) :-
        enumerated_junction(J1X),
        J1X=j(Seq,Pos,Str),
        Pos2 is Pos+1,
        J2X=j(Seq,Pos2,Str),
        junction(J2X),
        junction_uri(J1X,J1),
        junction_uri(J2X,J2).


% TODO: make optional
