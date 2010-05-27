:- module(simmatrix_multiset,
          [
	   aset_frequency/2,
	   aset_prob/2,
	   feature_pair_aset_pair_lcs/5,
	   feature_pair_aset_pair_lcs_ic/6
          ]).

:- use_module(bio(index_util)).
:- use_module(bio(ontol_db)). % todo
:- use_module(bio(bioprolog_util),[solutions/3]).


:- multifile feature_aset/2.
:- multifile subsumed_by/2.

subsumed_by(A,B) :- ontol_db:subclassRT(A,B).

aset_nr(A,A).

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


%% feature_subsumed_by_aset(?F,+S:set)
% true is all members of S hold for F
feature_subsumed_by_aset(F,S) :-
	feature_aset(F,S1),
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
	aset_nr(LCS_Set_1,LCS_Set).


feature_pair_aset_pair_lcs(F1,F2,S1,S2,LCS) :-
	feature_aset(F1,S1),
	feature_aset(F2,S2),
	aset_pair_lcs(S1,S2,LCS).

feature_pair_aset_pair_lcs_ic(F1,F2,S1,S2,LCS,IC) :-
	feature_pair_aset_pair_lcs(F1,F2,S1,S2,LCS),
	aset_ic(LCS,IC).

feature_pair_aset_pair_ic(F1,F2,S1,S2,IC) :-
	feature_pair_aset_pair_lcs_ic(F1,F2,S1,S2,_,IC).

%feature_pair_aset_subsumers(F1,F2,L) :-
%	setof(LCS-IC,S1^S2^feature_pair_aset_pair_lcs_ic(F1,F2,S1,S2,LCS,IC),SICs),
%	feature_
	




% ///?

%% feature_agroup(?F,?G) is nondet
% each feature has multiple attsets
% each attset has a unique id
feature_agroup(F,G) :-
	feature_aset(F,ASet),	% hook
	aset_agroup(ASet,G).

:- dynamic aset_ix/2.
aset_group(ASet,G) :-
	aset_ix(ASet,G).
aset_group(ASet,G) :-
	\+ aset_ix(ASet,_),
	gensym(agroup,G),
	assert(aset_ix(ASet,G)).

agroup_subsumed_by(GX,GY) :-
	aset_group(GX,SX),
	aset_group(GY,SY),
	aset_subsumed_by(SX,SY).

feature_pair_agroup_pair_lcs(F1,F2,G1,G2,LCS_Set) :-
	feature_agroup(F1,G1),
	feature_agroup(F2,G2),
	agroup_pair_lcs(G1,G2,LCS_Set).
agroup_ic(G,IC) :-
	agroup_prob(G,Prob),
	IC is -(log(Prob)/log(2)).
agroup_prob(G,Prob) :-
	agroup_frequency(G,Freq),
	feature_count(Tot),
	Prob is Freq / Tot.
agroup_frequency(G,Freq) :-
	aggregate(count,F,feature_subsumed_by_agroup(F,G),Freq).


bitwise_union(L,V) :-
	bitwise_union(L,V,0).

bitwise_union([],V,V) :- !.
bitwise_union([V1|L],V,V2) :-
	V3 is V1 \/ V2,
	bitwise_union(L,V,V3).
