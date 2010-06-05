annot_count(T,Num):-
        Num is count_distinct(G,curation_statement(_,G,_,T)).

curation_statement_evidence_type(A,G,T,E,ET):-
	curation_statement(A,G,_,T),curation_evidence(A,E),evidence_type(E,ET).

curation_statement_evidence_iss(A,G,T,E):-
	curation_statement_evidence_type(A,G,T,E,'ISS').

lonely_iss_direct(G,T,E):-
	curation_statement_evidence_iss(_,G,T,E),
	homologset_member(Hs,G),
	not((homologset_member(Hs,G2),
	     curation_statement_evidence_iss(_,G2,T,_),
	     not(G=G2))).

lonely_iss(G,T,E):-
	curation_statement_evidence_iss(_,G,T,E),
	homologset_member(Hs,G),
	not((homologset_member(Hs,G2),
	     curation_statement(_,G2,T2,_),
	     parentRT(T2,T),
	     not(G=G2))).

stale_iss(G,T,E):-
	curation_statement_evidence_iss(_,G,T,E),
	evidence_with(E,With),
	not((curation_statement(_,With,T2,_),
	     parentRT(T2,T))).



