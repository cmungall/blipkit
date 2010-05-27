:- module(simmatrix_multiset,
          [
	   create_sim_index/1,
	   feature_aset/2,
	   feature_nr_aset/2,
	   aset_frequency/2,
	   aset_prob/2,
	   feature_pair_aset_pair_lcs/5,
	   feature_pair_aset_pair_lcs_ic/6
          ]).

:- use_module(bio(index_util)).
:- use_module(bio(tabling)).
:- use_module(bio(ontol_db)). % todo
:- use_module(bio(bioprolog_util),[solutions/3]).

:- multifile feature_aset/2.
:- multifile subsumed_by/2.

:- multifile simmatrix_multiset:index_hook/1.

create_sim_index(File) :-
	var(File),
	!.
create_sim_index(File) :-
	index_hooks,
	%table_pred(aset_subsumed_by/2),
	materialize_indexes_to_file([feature_aset(1,0),
				     feature_nr_aset(1,0),
				     feature(1),
				     feature_count(1)],File).


index_hooks :-
	index_hook(_),
	fail.
index_hooks.

feature_nr_aset(F,ASet) :-
	setof(ASet,feature_aset(F,ASet),ASets),
	asets_extract_nr(ASets,ASetsNR),
	member(ASet,ASetsNR).

asets_extract_nr(ASets,ASetsNR) :-
	setof(ASet,
	      (	  member(ASet,ASets),
		  \+ aset_redundant_with_aset_set(ASet,ASets)),
	      ASetsNR).

aset_redundant_with_aset_set(ASet,ASets) :-
	member(ASet2,ASets),
	ASet2\=ASet,
	aset_subsumed_by(ASet2,ASet),
	\+ aset_subsumed_by(ASet,ASet2).



attribute_pair_lcs(A1,A2,CS) :-
	attribute_pair_cs(A1,A2,CS),
	\+ ((attribute_pair_cs(A1,A2,CS_2),
	     CS_2\=CS,
	     subsumed_by(CS_2,CS))).

attribute_pair_cs(A1,A2,CS) :-
	subsumed_by(A1,CS),
	subsumed_by(A2,CS).

aset_subsumed_by(SX,SY) :-
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


%% feature_subsumed_by_aset(?F,+S:set)
% true is all members of S hold for F
feature_subsumed_by_aset(F,S) :-
	feature_nr_aset(F,S1),
	aset_subsumed_by(S1,S).


%% aset_frequency(+S:set,?Freq:int)
% number of features that satisfy S
aset_frequency(S,Freq) :-
	aggregate(count,F,feature_subsumed_by_aset(F,S),Freq).

feature(F) :-
	setof(F,S^feature_aset(F,S),Fs),
	member(F,Fs).

feature_count(Freq) :-
	aggregate(count,F,feature(F),Freq).


aset_prob(S,Prob) :-
	aset_frequency(S,Freq),
	feature_count(Tot),
	Prob is Freq / Tot.

aset_ic(S,IC) :-
	aset_prob(S,Prob),
	IC is -(log(Prob)/log(2)).

aset_pair_lcs(S1,S2,LCS_Set) :-
	setof(A_LCS,A1^A2^(member(A1,S1),
			   member(A2,S2),
			   attribute_pair_lcs(A1,A2,A_LCS)),
	      LCS_Set_1),
	attributes_extract_nr(LCS_Set_1,LCS_Set).


%% feature_pair_aset_pair_lcs(?F1,?F2,?S1,?S2,?LCS) is nondet
% 
% if two features are specified the mode is:
% feature_pair_aset_pair_lcs(+F1,+F2,?S1,?S2,?LCS) det
%
% given two features, each of which may have multiple asets
% associated with it, enumerate LCS Asets.
feature_pair_aset_pair_lcs(F1,F2,S1,S2,LCS) :-
	feature_nr_aset(F1,S1),
	feature_nr_aset(F2,S2),
	debug(sim,'comparing ~w ~w VS ~w ~w',[F1,S1,F2,S2]),
	aset_pair_lcs(S1,S2,LCS).

feature_pair_aset_pair_lcs_ic(F1,F2,S1,S2,LCS,IC) :-
	feature_pair_aset_pair_lcs(F1,F2,S1,S2,LCS),
	aset_ic(LCS,IC).

feature_pair_aset_pair_ic(F1,F2,S1,S2,IC) :-
	feature_pair_aset_pair_lcs_ic(F1,F2,S1,S2,_,IC).

feature_pair_maxIC(F1,F2,MaxIC) :-
	feature_pair_bestLCS_maxIC(F1,F2,_,MaxIC).

feature_pair_bestLCS_maxIC(F1,F2,BestLCS,MaxIC) :-
	setof(IC-LCS,feature_pair_aset_pair_lcs_ic(F1,F2,S1,S2,LCS,IC),IC_LCSs),
	reverse(IC_LCSs,[MaxIC-BestLCS|_]).




%feature_pair_aset_subsumers(F1,F2,L) :-
%	setof(LCS-IC,S1^S2^feature_pair_aset_pair_lcs_ic(F1,F2,S1,S2,LCS,IC),SICs),
%	feature_
	


