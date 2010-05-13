:- relation(starts_during,start).
:- relation(ends_during,end).

starts_during_or_after(P,S) <- starts_during(P,S). % do we need if part is reflexive??
starts_during_or_after(P,S) <- starts_during(P,S2),preceded_by(S2,S). % S2 is the later stage
starts_during_or_after(P,S) <- part_of(P,Q),starts_during(Q,S).

ends_during_or_before(P,S) <- ends_during(P,S).
ends_during_or_before(P,S) <- ends_during(P,S1),preceded_by(S,S1). % S1 is the later stage
ends_during_or_before(P,S) <- part_of(P,Q),ends_during(Q,S).

unsatisfiable(P) <- starts_during_or_after(P,S2),ends_during_or_before(P,S1).

starts_during(P,S) <- starts_during(P,Sx),simultaneous_with(S,Sx).
ends_during(P,S) <- ends_during(P,Sx),simultaneous_with(S,Sx).

inst_rel(X,arises_from,Y) <- inst_rel(X,succeeds,Y).
inst_rel(X,arises_from,Y) <- inst_rel(X,buds_from,Y).
inst_rel(X,arises_from,Z) <- inst_rel(X,arises_from,Y),inst_rel(Y,succeeds,Z).
inst_rel(X,arises_from,Z) <- inst_rel(X,arises_from,Y),inst_rel(Y,buds_from,Z).

