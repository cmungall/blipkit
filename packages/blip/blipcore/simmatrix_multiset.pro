:- module(simmatrix_multiset,
          [
	   create_sim_index/1,
	   feature_attx/2,
	   feature_nr_attx/2,
	   attx_frequency/2,
	   attx_prob/2,
	   feature_pair_attx_pair_LCS/5,
	   feature_pair_attx_pair_LCS_IC/6,
	   feature_pair_attx_pair_LCS_simJ/6,
	   feature_pair_bestLCS_maxIC/4,
	   feature_pair_all_LCS_avg_IC/4,
	   feature_pair_no_LCS_attx/4,
	   feature_pair_score_value/4
          ]).

:- use_module(library(ordsets)).
:- use_module(bio(index_util)).
:- use_module(bio(tabling)).
:- use_module(bio(ontol_db)). % todo
:- use_module(bio(bioprolog_util),[solutions/3]).

%% feature_nr_attx(F,Attx)
%
% mapping between a feature and multiple attribute-expressions (Attxs).
% An attx is a conjunction of attributes, represented as a prolog set.
% A set [A1,A2,...,An] has the interpretation A1^An^...^An.
:- multifile feature_attx/2.
:- multifile subsumed_by/2.

:- multifile simmatrix_multiset:index_hook/1.

create_sim_index(File) :-
	var(File),
	!.
create_sim_index(File) :-
	index_hooks,
	table_pred(subsumed_by/2),
	table_pred(attribute_pair_cs/2),
	table_pred(attribute_pair_LCS/2),
	table_pred(attx_frequency/2), % doubles the speed
	%table_pred(attx_i_subsumed_by/2),
	%table_pred(attx_id/2),
	%table_pred(attx_subsumed_by/2),
	materialize_indexes_to_file([feature_attx(1,0),
				     feature_nr_attx(1,0),
				     feature(1),
				     feature_count(1),
				     atomic_attr(1)
				    %attribute_pair_LCS(1,0,0) % optional?
				    ],
				    File).

full_index(File) :-
		materialize_indexes_to_file([
					     feature_pair_attx_pair_LCS/5,
					     feature_pair_attx_pair_LCS_IC/6
					    ],
					    File).

					     
					     
index_hooks :-
	index_hook(_),
	fail.
index_hooks.

%% feature_nr_attx(F,Attx)
% as feature_attx/2, but remove redundant attxs
feature_nr_attx(F,Attx) :-
	setof(Attx,feature_attx(F,Attx),Attxs),
	attxs_extract_nr(Attxs,AttxsNR),
	member(Attx,AttxsNR).

attxs_extract_nr(Attxs,AttxsNR) :-
	setof(Attx,
	      (	  member(Attx,Attxs),
		  \+ attx_redundant_with_attx_set(Attx,Attxs)),
	      AttxsNR).

attx_redundant_with_attx_set(Attx,Attxs) :-
	member(Attx2,Attxs),
	Attx2\=Attx,
	attx_subsumed_by(Attx2,Attx),
	\+ attx_subsumed_by(Attx,Attx2).

%% attribute_pair_LCS(+A1,+A2,?LCS)
% standard LCS between atomic attributes.
% consider indexing.
attribute_pair_LCS(A1,A2,CS) :-
	atomic_attr(A1),
	atomic_attr(A2),
	debug(lcs,'testing ~w vs ~w',[A1,A2]),
	attribute_pair_cs(A1,A2,CS),
	\+ ((attribute_pair_cs(A1,A2,CS_2),
	     CS_2\=CS,
	     subsumed_by(CS_2,CS))).

attribute_pair_cs(A1,A2,CS) :-
	subsumed_by(A1,CS),
	subsumed_by(A2,CS).

%% attx_subsumed_by(+SX,+SY) is semidet
% true if SY subsumes SX.
% this is true if each element of SY holds for SX
attx_subsumed_by(SX,SY) :-
	forall(member(B,SY),
	       (   member(A,SX),
		   subsumed_by(A,B))). % hook

%% attributes_extract_nr(+As:list,?AsNR:list)
% remove all redundant attributes
attributes_extract_nr(As,AsNR) :-
	setof(A,
	      (	  member(A,As),
		  \+ attribute_redundant_with_set(A,As)),
	      AsNR).

attribute_redundant_with_set(A,As) :-
	member(A2,As),
	A2\=A,
	subsumed_by(A2,A),
	\+ subsumed_by(A,A2).


%% feature_subsumed_by_attx(?F,+S:set)
% true is all members of S hold for F
feature_subsumed_by_attx(F,S) :-
	feature_nr_attx(F,S1),
	attx_subsumed_by(S1,S).


%% attx_frequency(+S:set,?Freq:int)
% number of features that satisfy S.
% note: in future we may want option for treatment of non-independent features;
% grouping multiple observations together.
% for example: multiple individuals with PD have repeating phenotypes.
% for now, we push responsibility on the user to do accurate grouping
attx_frequency(S,Freq) :-
	aggregate(count,F,feature_subsumed_by_attx(F,S),Freq).

feature(F) :-
	setof(F,S^feature_attx(F,S),Fs),
	member(F,Fs).

feature_count(Freq) :-
	aggregate(count,F,feature(F),Freq).

atomic_attr(A) :-
	setof(A,F^AX^(feature_nr_attx(F,AX),member(A,AX)),As),
	member(A,As).


attx_prob(S,Prob) :-
	attx_frequency(S,Freq),
	feature_count(Tot),
	Prob is Freq / Tot.

attx_IC(S,IC) :-
	attx_prob(S,Prob),
	IC is -(log(Prob)/log(2)).

attx_atomic_subsumer(X,P) :-
	member(A,X),
	subsumed_by(A,P).

attx_atomic_subsumers(X,Ps) :-
	setof(P,attx_atomic_subsumer(X,P),Ps).


attx_pair_simJ(X1,X2,SimJ) :-
	attx_atomic_subsumers(X1,X1Ps),
	attx_atomic_subsumers(X2,X2Ps),
	ord_union(X1Ps,X2Ps,U), % consider bit-vectors
	ord_intersection(X1Ps,X2Ps,I),
	length(U,NumU),
	length(I,NumI),
	SimJ is NumI/NumU.
	
%% attx_pair_LCS(+S1,+S2,?LCS_Set)
% given two attribute-expressions, find a minimal subsuming expression.
attx_pair_LCS(S1,S2,LCS_Set) :-
	setof(A_LCS,A1^A2^(member(A1,S1),
			   member(A2,S2),
			   attribute_pair_LCS(A1,A2,A_LCS)),
	      LCS_Set_1),
	attributes_extract_nr(LCS_Set_1,LCS_Set).


%% feature_pair_attx_pair_LCS(?F1,?F2,?S1,?S2,?LCS) is nondet
% 
% if two features are specified the mode is:
% feature_pair_attx_pair_LCS(+F1,+F2,?S1,?S2,?LCS) det
%
% given two features, each of which may have multiple attxs
% associated with it, enumerate LCS Attxs.
feature_pair_attx_pair_LCS(F1,F2,S1,S2,LCS) :-
	feature_nr_attx(F1,S1),
	feature_nr_attx(F2,S2),
	debug(sim,'comparing ~w ~w VS ~w ~w',[F1,S1,F2,S2]),
	attx_pair_LCS(S1,S2,LCS).

%% feature_pair_attx_pair_LCS_IC(?F1,?F2,?S1,?S2,?LCS,?IC) is nondet
% as feature_pair_attx_pair_LCS_IC/5, but include the IC of the LCS
feature_pair_attx_pair_LCS_IC(F1,F2,S1,S2,LCS,IC) :-
	feature_pair_attx_pair_LCS(F1,F2,S1,S2,LCS),
	attx_IC(LCS,IC).

%% feature_pair_attx_pair_LCS_simJ(?F1,?F2,?S1,?S2,?LCS,?simJ) is nondet
% as feature_pair_attx_pair_LCS_simJ/5, but include the simJ of the LCS
feature_pair_attx_pair_LCS_simJ(F1,F2,S1,S2,LCS,SimJ) :-
	feature_pair_attx_pair_LCS_IC(F1,F2,S1,S2,LCS,_), % needed?
	attx_pair_simJ(S1,S2,SimJ). % TODO

%% feature_pair_attx_pair_IC(?F1,?F2,?S1,?S2,?IC)
% as feature_pair_attx_pair_LCS_IC/5, but include the IC of the LCS rather than the LCS itself
feature_pair_attx_pair_IC(F1,F2,S1,S2,IC) :-
	feature_pair_attx_pair_LCS_IC(F1,F2,S1,S2,_,IC).

feature_pair_attx_best_LCS_IC(F1,F2,LCS,IC,S1,S2) :-
	feature_pair_attx_pair_LCS_IC(F1,F2,S1,S2,LCS,IC),
	% best match for S1
	\+ ((feature_pair_attx_pair_LCS_IC(F1,F2,S1,_,_,Better_IC),
	     Better_IC > IC)).
feature_pair_attx_best_LCS_IC(F1,F2,LCS,IC,S1,S2) :-
	feature_pair_attx_pair_LCS_IC(F1,F2,S1,S2,LCS,IC),
	% best match for S2
	\+ ((feature_pair_attx_pair_LCS_IC(F1,F2,_,S2,_,Better_IC),
	     Better_IC > IC)).

%% feature_pair_attx_best_LCS_simJ(+F1,+F2,?LCS,?SimJ,?S1,?S2) is nondet
% every feature pair as a number of LCSs that are best-groupings for one of the attxs
feature_pair_attx_best_LCS_simJ(F1,F2,LCS,SimJ,S1,S2) :-
	feature_pair_attx_pair_LCS_simJ(F1,F2,S1,S2,LCS,SimJ),
	% best match for S1
	\+ ((feature_pair_attx_pair_LCS_simJ(F1,F2,S1,_,_,Better_simJ),
	     Better_simJ > SimJ)).
feature_pair_attx_best_LCS_simJ(F1,F2,LCS,SimJ,S1,S2) :-
	feature_pair_attx_pair_LCS_simJ(F1,F2,S1,S2,LCS,SimJ),
	% best match for S2
	\+ ((feature_pair_attx_pair_LCS_simJ(F1,F2,_,S2,_,Better_simJ),
	     Better_simJ > SimJ)).

%% feature_pair_all_LCS_avg_IC(F1,F2,Matches,AvgIC)
% 
feature_pair_all_LCS_avg_IC(F1,F2,RevScores,AvgIC) :-
	setof(IC-s(LCS,S1,S2),feature_pair_attx_best_LCS_IC(F1,F2,LCS,IC,S1,S2),Scores),
	reverse(Scores,RevScores),
	length(RevScores,Num),
	findall(IC,member(IC-_,RevScores),ICs),
	sumlist(ICs,TotalIC),
	AvgIC is TotalIC / Num.

%% feature_pair_all_LCS_avg_simJ(F1,F2,Matches,AvgSimJ)
% Only LCSs that are the best simJ-LCS for at least one attx
feature_pair_all_LCS_avg_simJ(F1,F2,RevScores,AvgSimJ) :-
	setof(SimJ-s(LCS,S1,S2),feature_pair_attx_best_LCS_simJ(F1,F2,LCS,SimJ,S1,S2),Scores),
	reverse(Scores,RevScores),
	length(RevScores,Num),
	findall(SimJ,member(SimJ-_,RevScores),SimJs),
	sumlist(SimJs,TotalSimJ),
	AvgSimJ is TotalSimJ / Num.

feature_pair_minimal_LCS_set_by_simJ(F1,F2,RevScores,AvgSimJ) :-
	setof(SimJ-LCS,S1^S2^feature_pair_attx_best_LCS_simJ(F1,F2,LCS,SimJ,S1,S2),ScoreLCSPairs),
	setof(SimJ-lcs(LCS,Xs1,Xs2),
	      (	  member(SimJ-LCS,ScoreLCSPairs),
		  feature_pair_attx_best_LCS_simJ(F1,F2,LCS,SimJ,X1,SX)),
	      Scores),
	% unmatched
	setof(U1,
	      (	  feature_nr_attx(F1,U1),
		  \+ feature_pair_attx_best_LCS_simJ(F1,F2,_,_,U1,_)),
	      Us1),
	% unmatched
	setof(U1,
	      (	  feature_nr_attx(F2,U2),
		  \+ feature_pair_attx_best_LCS_simJ(F1,F2,_,_,_,U2)),
	      Us2),
	reverse([0-lcs([],Us1,Us2)|Scores],RevScores),
	findall(SimJ,
		(   member(SimJ-lcs(_,X1s,X2s)-Scores),
		    (	member(_,X1s)
		    ;	member(_,X2s))),
		SimJs),
	length(SimJs,Len),
	sumlist(SimJs,Sum),
	AvgSimJ is Sum/Len.


%% feature_pair_maxIC(?F1,?F2,?MaxIC)
% of all LCSs of all attx of F1 and F2, MaxIC is the best
feature_pair_maxIC(F1,F2,MaxIC) :-
	feature_pair_bestLCS_maxIC(F1,F2,_,MaxIC).

%% feature_pair_maxIC(?F1,?F2,?BestLCSs:list,?MaxIC) is det
% as feature_pair_maxIC/3, include the LCS that has the best IC
feature_pair_bestLCS_maxIC(F1,F2,BestLCSs,MaxIC) :-
	setof(IC-LCS,S1^S2^feature_pair_attx_pair_LCS_IC(F1,F2,S1,S2,LCS,IC),IC_LCSs),
	reverse(IC_LCSs,[MaxIC-_|_]),
	setof(BestLCS,member(MaxIC-BestLCS,IC_LCSs),BestLCSs).


feature_pair_score_value(F1,F2,S,V) :-
	feature_pair_score_value_asym(F1,F2,S,V).
feature_pair_score_value(F1,F2,S,V) :-
	feature_pair_score_value_asym(F2,F1,S,V).


feature_pair_score_value_asym(F1,F2,maxIC,MaxIC) :-
	feature_pair_bestLCS_maxIC(F1,F2,_,MaxIC).
feature_pair_score_value_asym(F1,F2, best_LCS-maxIC, Best_LCS-MaxIC) :-
	feature_pair_bestLCS_maxIC(F1,F2,Best_LCS,MaxIC).
feature_pair_score_value_asym(F1,F2, best_LCS, Best_LCS) :-
	feature_pair_bestLCS_maxIC(F1,F2,Best_LCS,_).
feature_pair_score_value_asym(F1,F2, all_LCS-avg_IC, All-AvgIC) :-
	feature_pair_all_LCS_avg_IC(F1,F2,All,AvgIC).
feature_pair_score_value_asym(F1,F2, avg_IC, AvgIC) :-
	feature_pair_all_LCS_avg_IC(F1,F2,_,AvgIC).
feature_pair_score_value_asym(F1,F2, all_LCS-avg_simJ, All-AvgSimJ) :-
	feature_pair_all_LCS_avg_simJ(F1,F2,All,AvgSimJ).
feature_pair_score_value_asym(F1,F2, minimal_LCS_simJ-avg_simJ, All-AvgSimJ) :-
	feature_pair_minimal_LCS_set_by_simJ(F1,F2,All,AvgSimJ).
feature_pair_score_value_asym(F1,F2, no_LCS(Fn), SL) :-
	setof(S,feature_pair_no_LCS_attx(F1,F2,Fn,S),SL).

% not all can be matched
feature_pair_no_LCS_attx(F1,F2,f1,S1) :-
	feature_nr_attx(F1,S1),
	\+ feature_pair_attx_pair_LCS_IC(F1,F2,S1,_,_,_).
feature_pair_no_LCS_attx(F1,F2,f2,S2) :-
	feature_nr_attx(F2,S2),
	\+ feature_pair_attx_pair_LCS_IC(F1,F2,_,S2,_,_).
	

%feature_pair_attx_subsumers(F1,F2,L) :-
%	setof(LCS-IC,S1^S2^feature_pair_attx_pair_LCS_IC(F1,F2,S1,S2,LCS,IC),SICs),
%	feature_
	


% experimental
attx_id(S,ID) :-
	sformat(ID,'~w',[S]).

% experimental
indexable_attx_subsumed_by(SX,SY) :-
	attx_id(SX,SXI),
	attx_id(SY,SYI),
	attx_i_subsumed_by(SXI,SYI).

% experimental
attx_i_subsumed_by(SXI,SYI) :-
	attx_id(SX,SXI),
	attx_id(SY,SYI),
	attx_subsumed_by_impl(SX,SY).
