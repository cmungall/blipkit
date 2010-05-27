:- module(simmatrix_multiset,
          [
	   create_sim_index/1,
	   feature_attx/2,
	   feature_nr_attx/2,
	   attx_frequency/2,
	   attx_prob/2,
	   feature_pair_attx_pair_lcs/5,
	   feature_pair_attx_pair_lcs_ic/6
          ]).

:- use_module(bio(index_util)).
:- use_module(bio(tabling)).
:- use_module(bio(ontol_db)). % todo
:- use_module(bio(bioprolog_util),[solutions/3]).

:- multifile feature_attx/2.
:- multifile subsumed_by/2.

:- multifile simmatrix_multiset:index_hook/1.

create_sim_index(File) :-
	var(File),
	!.
create_sim_index(File) :-
	index_hooks,
	table_pred(attx_i_subsumed_by/2),
	table_pred(attx_id/2),
	%table_pred(attx_subsumed_by/2),
	materialize_indexes_to_file([feature_attx(1,0),
				     feature_nr_attx(1,0),
				     feature(1),
				     feature_count(1)],
				    File).

index_hooks :-
	index_hook(_),
	fail.
index_hooks.

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

attribute_pair_lcs(A1,A2,CS) :-
	attribute_pair_cs(A1,A2,CS),
	\+ ((attribute_pair_cs(A1,A2,CS_2),
	     CS_2\=CS,
	     subsumed_by(CS_2,CS))).

attribute_pair_cs(A1,A2,CS) :-
	subsumed_by(A1,CS),
	subsumed_by(A2,CS).

% experimental
attx_id(S,ID) :-
	sformat(ID,'~w',[S]).

% experimental
attx_subsumed_by(SX,SY) :-
	attx_id(SX,SXI),
	attx_id(SY,SYI),
	attx_i_subsumed_by(SXI,SYI).

% experimental
attx_i_subsumed_by(SXI,SYI) :-
	attx_id(SX,SXI),
	attx_id(SY,SYI),
	attx_subsumed_by_impl(SX,SY).

attx_subsumed_by_impl(SX,SY) :-
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
% number of features that satisfy S
attx_frequency(S,Freq) :-
	aggregate(count,F,feature_subsumed_by_attx(F,S),Freq).

feature(F) :-
	setof(F,S^feature_attx(F,S),Fs),
	member(F,Fs).

feature_count(Freq) :-
	aggregate(count,F,feature(F),Freq).


attx_prob(S,Prob) :-
	attx_frequency(S,Freq),
	feature_count(Tot),
	Prob is Freq / Tot.

attx_ic(S,IC) :-
	attx_prob(S,Prob),
	IC is -(log(Prob)/log(2)).

attx_pair_lcs(S1,S2,LCS_Set) :-
	setof(A_LCS,A1^A2^(member(A1,S1),
			   member(A2,S2),
			   attribute_pair_lcs(A1,A2,A_LCS)),
	      LCS_Set_1),
	attributes_extract_nr(LCS_Set_1,LCS_Set).


%% feature_pair_attx_pair_lcs(?F1,?F2,?S1,?S2,?LCS) is nondet
% 
% if two features are specified the mode is:
% feature_pair_attx_pair_lcs(+F1,+F2,?S1,?S2,?LCS) det
%
% given two features, each of which may have multiple attxs
% associated with it, enumerate LCS Attxs.
feature_pair_attx_pair_lcs(F1,F2,S1,S2,LCS) :-
	feature_nr_attx(F1,S1),
	feature_nr_attx(F2,S2),
	debug(sim,'comparing ~w ~w VS ~w ~w',[F1,S1,F2,S2]),
	attx_pair_lcs(S1,S2,LCS).

feature_pair_attx_pair_lcs_ic(F1,F2,S1,S2,LCS,IC) :-
	feature_pair_attx_pair_lcs(F1,F2,S1,S2,LCS),
	attx_ic(LCS,IC).

feature_pair_attx_pair_ic(F1,F2,S1,S2,IC) :-
	feature_pair_attx_pair_lcs_ic(F1,F2,S1,S2,_,IC).

feature_pair_maxIC(F1,F2,MaxIC) :-
	feature_pair_bestLCS_maxIC(F1,F2,_,MaxIC).

feature_pair_bestLCS_maxIC(F1,F2,BestLCS,MaxIC) :-
	setof(IC-LCS,S1^S2^feature_pair_attx_pair_lcs_ic(F1,F2,S1,S2,LCS,IC),IC_LCSs),
	reverse(IC_LCSs,[MaxIC-BestLCS|_]).




%feature_pair_attx_subsumers(F1,F2,L) :-
%	setof(LCS-IC,S1^S2^feature_pair_attx_pair_lcs_ic(F1,F2,S1,S2,LCS,IC),SICs),
%	feature_
	


