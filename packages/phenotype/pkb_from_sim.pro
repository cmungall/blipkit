:- module(pkb_from_sim,
	  []).


:- use_module(bio(simmatrix_multiset)).
:- use_module(bio(pkb_db)).

% note: these will be computed dynamically, unless a fact file is provided.
pkb_db:organism_pair_score_value(F1,F2,S,V) :-
	feature_pair_score_value(F1,F2,S,V).

pkb_db:organism_pair_all_scores(F1,F2,Scores) :-
	setof(S,F1^F2^V^feature_pair_score_value(F1,F2,S,V),Scores).

pkb_db:phenotype_pair_score_value(X1,X2,lcs-lcs_IC,LCS-IC) :-
	feature_pair_attx_pair_LCS_IC(_,_,X1,X2,LCS,IC).
pkb_db:phenotype_pair_score_value(X1,X2,lcs_IC,IC) :-
	feature_pair_attx_pair_LCS_IC(_,_,X1,X2,_,IC).

