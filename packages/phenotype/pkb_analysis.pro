
:- use_module(pkb_db).
:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).
:- use_module(bio(index_util)).
:- use_module(library(thea2/owl2_graph_reasoner)).

% hacky: model organism or instance or class
organism_category(F,SL) :-
	organism_species(F,S),
	species_label(S,SL),
	SL\=human.
organism_category(F,SL) :-
	organism_species(F,S),
	species_label(S,human),
	(   organism_role_disease(F,_,_)
	->  SL=inst
	;   SL=class).

% score + categories (human,mouse,etc) for a pair
feature_pair_category_pair_ci(F1,F2,S1,S2,Sc) :-
	feature_pair_ci_cu_simj(F1,F2,_,_,Sc),
	organism_category(F1,S1),
	organism_category(F2,S2).



% only compare two features if they are good matches for the species
comparable_feature_pair(F1,F2,Rank,Len) :-
	feature_pair_category_pair_ci(F1,F2,S1,S2,Sc),
	setof(ScX-F2X,
	      feature_pair_category_pair_ci(F1,F2X,S1,S2,ScX),
	      ScoredPairs),
	reverse(ScoredPairs,RevScoredPairs),
	nth1(Rank,RevScoredPairs,Sc-F2),
	length(ScoredPairs,Len),
	Marker is Len/3,
	Rank < Marker.

% standardize direction, no dupes, at least 1
fp(F1,F2) :-
	comparable_feature_pair(F1,F2,_,_),
	F1 @< F2.

fp(F1,F2) :-
	comparable_feature_pair(F2,F1,_,_),
	F1 @< F2.

:- multifile cached_feature_pair_attx_pair_LCS_IC/5.

% use this for pkb
generate_all(Goal) :-
	Goal=feature_pair_attx_pair_LCS_IC(_F1,_F2,_S1,_S2,_LCS,IC),
	Goal,
	IC >= 3.5.

generate_selected(Goal) :-
	setof(F1-F2,fp(F1,F2),Pairs),
        length(Pairs,NumPairs),
	member(F1-F2,Pairs),
	debug(foo,'test: ~w vs ~w // of ~w',[F1,F2,NumPairs]),
	Goal=feature_pair_attx_pair_LCS_IC(F1,F2,_S1,_S2,LCS,IC),
	debug(foo,'  **comparing: ~w vs ~w',[F1,F2]),
	Goal,
	debug(foo,'     **result: ~w :: ~w',[IC,LCS]),
	IC >= 2.5.

prepare(File) :-
        graph_reasoner_memoize,
	create_sim_index(File),
	materialize_index(comparable_feature_pair(1,0,0,0)).


