:- module(simmatrix,
          [
           all_by_all/1,
           all_by_all/0,
           generate_term_indexes/3,
           compare_feature_pair/4,
	   attribute_feature_count/2,
	   attribute_information_content/2,
           feature_attribute/2,
           feature_attributeset/2,
           feature_exists/1,
           attribute_exists/1,
           feature_pair_ci/3,
           feature_pair_cu/3,
	   feature_pair_subset_of/3,
           feature_pair_simj/3,
	   feature_pair_ci_cu_simj/5,
           feature_pair_simj_of_f1/3,
           feature_pair_simj_of_f2/3,
           feature_pair_simGIC/3,
	   feature_pair_simICratio/3,
           feature_pair_maxIC/3,
           feature_pair_maxIC_attributes/4,
           feature_pair_cossim/3,
           feature_pair_avgICCS/3,
           feature_pair_avgICCS/4,
           feature_pair_nr_ICatt_pairs/4,
           feature_pair_nr_independent_atts/4,
           feature_pair_nr_independent_atts_corrected/6,
           attributeset_pair_csumIC_atts/6,
           feature_pair_pval_hyper/3,
           feature_pair_pval_hyper/7,
           attribute_pair_pval_hyper/3,
           attribute_pair_pval_hyper/7,
           feature_pair_all_scores/3,
           feature_vector/2,
           feature_matches/2,
           feature_matches/3,
	   vector_attributes/2,
	   av_subsumes_feature/2,
           rksort_with_limit/3
          ]).

:- use_module(bio(bioprolog_util),[solutions/3]).
:- use_module(bio(stats_distributions)). % hypergeometric

:- dynamic feature_ix/2.
:- dynamic attribute_ix/2.
:- dynamic feature_vector_cached/2.
:- dynamic attribute_vector_cached/2.
:- dynamic template/3.
:- dynamic feature_attribute/2.
:- dynamic feature_attributeset_cached/2.
:- dynamic feature_pair_score_cached/3.
:- dynamic attribute_subsumer_vector_cached/2. % new

:- multifile simmatrix:generate_term_indexes_hook/1.

%% attribute_subsumer(?Attribute,?ParentAttribute)
% must be asserted or loaded if using ICCS
%% feature_attribute_direct(?Feature,?Attribute)
% must be asserted or loaded if using ICCS

% todo
write_indexes :-
        forall(member(P/A,
                      [feature_ix/2,
                       attribute_ix/2,
                       feature_vector_cached/2,
                       template/3,
                       feature_pair_score_cached/3]),
               (   functor(Term,P,A),
                   forall(Term,format('~q.~n',[Term])))).

        


%% generate_term_index(+Base,+Template,+Goal)
%
% for every unique value of Template for Goal,
% assign an integer, commencing with 0.
% 
% Base = attribute_ix | feature_ix.
%
% e.g. generate_term_index(feature_ix,G,g2p(G,P))
%   generates: feature_ix(G,GID:int)
%
% e.g. generate_term_index(attribute_ix,P,g2p(G,P))
%   generates: attribute_ix(P,PID:int)
%
% the dynamic fact-predicate Base/2 is populated after
% generate_term_index/3 succeeds. This maps an arbitrary term
% (recommended to be an atom) to a position in a vector, i.e.
%   Base(Atom,Int)
generate_term_index(Base,Template,Goal) :-
        G=..[Base,Template,Num],
        retractall(G),
        % this is a slightly ugly implementation,
        % but it avoids hitting the goal stack limit on
        % 32 bit machines
        nb_setval(n,0),
        forall(Goal,
               (   (   G        % already mapped?
                   ->  true     
                   ;   nb_getval(n,Num),
                       assert(G),
                       Num2 is Num+1,
                       nb_setval(n,Num2)))),
        nb_getval(n,Max),
        debug(sim,'generated ~w ix; num=~w',[Base,Max]),
        index_countvar(Base,CountVar),
        nb_setval(CountVar,Max).

index_countvar(feature_ix,feature_count).
index_countvar(attribute_ix,attribute_count).

%% feature_count(?NumFeatures:int)
feature_count(X) :- nb_getval(feature_count,X).

feature_exists(X) :- feature_ix(X,_).
attribute_exists(X) :- attribute_ix(X,_).


%% attribute_count(?NumAttributes:int)
attribute_count(X) :- nb_getval(attribute_count,X).

%% attribute_feature_count(?A,?NumFeatures:int)
% number of features that have attribute A
attribute_feature_count(A,Num) :-
        attribute_ix(A,_),
        template(F,A,Goal),
        aggregate(count,F,Goal^Goal,Num).

%% attribute_information_content(?A,?IC:float)
% -log2(p(A))
% note: you may wish to memoize this using
% the blipkit tabling module
attribute_information_content(A,IC) :-
        attribute_feature_count(A,NumFA),
        feature_count(NumF),
        IC is -(log(NumFA/NumF)/log(2)).

:- module_transparent all_by_all/4.
all_by_all(Feature,Attribute,Goal,SPairs) :-
        generate_term_indexes(Feature,Attribute,Goal),
        all_by_all(SPairs).

all_by_all(SPairs) :-
        findall(Num-pair(F1,F2),
                (   feature_ix(F1,Ix1),
                    feature_ix(F2,Ix2),
                    Ix1<Ix2,
                    compare_feature_pair(F1,F2,Num)),
                SPairs).

all_by_all :-
        retractall(feature_pair_score_cached/3),
        feature_ix(F1,Ix1),
        feature_ix(F2,Ix2),
        Ix1<Ix2,
        compare_feature_pair(F1,F2,Num),
        assert(feature_pair_score_cached(F1,F2,Num)),
        fail.
all_by_all :- true.

%% generate_term_indexes(+Feature:variable,+Attribute:variable,+Goal:goal)
% creates an initial index of all features and attributes,
% using Goal to specify the feature to attribute relation.
%
% mappings stored in feature_ix/2 and attribute_ix/2.
% these indexes do not need to be used directly by the library user,
% but they are used internally to map atoms to integer indexes
% for efficient lookup of attribute vectors
% Example: =|generate_term_indexes(F,A,curation_statementT(_,F,_,A))
:- module_transparent generate_term_indexes/3.
generate_term_indexes(Feature,Attribute,Goal) :-
        retractall(simmatrix:template(_,_,_)),
        assert(simmatrix:template(Feature,Attribute,Goal)),
        retractall(simmatrix:feature_attribute/2),
        forall(Goal,assert(simmatrix:feature_attribute(Feature,Attribute))),
        retractall(simmatrix:feature_vector_cached/2),
	debug(sim,'generating feature ix',[]),
        generate_term_index(feature_ix,Feature,Goal),
	debug(sim,'generating attribute ix',[]),
        generate_term_index(attribute_ix,Attribute,Goal).


%% feature_matches(?F:atom,?Matches:list)
% what does F match?
feature_matches(F,Matches) :-
        feature_matches(F,Matches,[]).

%% feature_matches(?F:atom,?Matches:list,+Opts:list)
% what does F match?
% Opts :
%   limit(Limit) - return top Limit hits
feature_matches(F,Matches,Opts) :-
        member(min_ov(MinOv),Opts),
        !,
        findall(Num-F2,
                (   compare_feature_pair(F,F2,Num,Opts),
                    Num>MinOv),
                Matches1),
        best_matches(Matches1,Matches,Opts).

feature_matches(F,Matches,Opts) :-
        findall(Num-F2,
                compare_feature_pair(F,F2,Num,Opts),
                Matches1),
        best_matches(Matches1,Matches,Opts).

best_matches(Matches,BestMatches,Opts) :-
        member(limit(Limit),Opts),
        !,
        rksort_with_limit(Matches,BestMatches,Limit).
best_matches(L,L2,_) :-
        keysort(L,SL),
        reverse(SL,L2).

rksort_with_limit(L,L2,Limit) :-
        keysort(L,SL),
        reverse(SL,RSL),
        take_n(RSL,L2,Limit).

take_n([],[],_) :- !.
take_n(_,[],0) :- !.
take_n([H|L],[H|L2],N) :-
        Nm1 is N-1,
        take_n(L,L2,Nm1).

av_subsumes_feature(AV,F) :-
	feature_vector(F,FV),
	VI is FV /\ AV,
	VI = AV.



%% feature_pair_ci(?F1,?F2,?Num:int)
% intersection of attribute values:
% | attrs(F1) ∩ attrs(F2) |
%
% equivalent to the dot product between
% the two attribute vectors
feature_pair_ci(F1,F2,Num) :-
        feature_vector(F1,AV1),
        feature_vector(F2,AV2),
        Num is popcount(AV1 /\ AV2).

%% feature_pair_cu(?F1,?F2,?Num:int)
% union of attribute values:
% | attrs(F1) ∪ attrs(F2) |
feature_pair_cu(F1,F2,Num) :-
        feature_vector(F1,AV1),
        feature_vector(F2,AV2),
        AVP = AV1 \/ AV2,
        Num is popcount(AVP).

%% feature_pair_subset_of(?F1,?F2,S:number)
% degree to which F1 is a subset of F2.
% 1.0 iff every attribute of F1 is an attribute of F2
% 0.0 iff no attributes of F1 are attributes of F2
feature_pair_subset_of(F1,F2,S) :-
        feature_vector(F1,AV1),
        feature_vector(F2,AV2),
        Num1 is popcount(AV1),
        NumBoth is popcount(AV1 /\ AV2),
	S is NumBoth/Num1.



%% feature_pair_simj(?F1,?F2,-Sim:float)
% Jacard similarity coefficient between two features,
% based on their attribute vectors:
% | A1 ∩ A2| / | A1 ∪ A2|
% where A1 and A2 are the sets of positive attributes
% in F1 and F2 respectively
%
% Speed: the underlying implementation uses bitwise
% operations on ints, so it should be super-fast
feature_pair_simj(F1,F2,Sim) :-
        feature_pair_cu(F1,F2,CU),
	CU > 0,
        feature_pair_ci(F1,F2,CI),
        Sim is CI/CU.

feature_pair_ci_cu_simj(F1,F2,CI,CU,Sim) :-
        feature_pair_cu(F1,F2,CU),
	CU > 0,
        feature_pair_ci(F1,F2,CI),
        Sim is CI/CU.

feature_pair_simj_of_f1(F1,F2,Sim) :-
        feature_vector(F1,AV),
        CU is popcount(AV),
	CU > 0,
        feature_pair_ci(F1,F2,CI),
        Sim is CI/CU.

feature_pair_simj_of_f2(F1,F2,Sim) :-
        feature_vector(F2,AV),
        CU is popcount(AV),
	CU > 0,
        feature_pair_ci(F1,F2,CI),
        Sim is CI/CU.


%% feature_pair_simGIC(?F1,?F2,-Sim:float)
% ∑ [a ∈ A1 ∩ A2] IC(a)  / ∑ [a ∈  A1 ∪ A2]IC(t)
% Ref:
% Pesquita, C.;Faria, D.;Bastos, H.;Falcão, AO.; Couto, FM. 
% Evaluating GO-based semantic similarity measures. Proceedings of the 10th Annual Bio-Ontologies Meeting (Bio-Ontologies 2007).
%
% Speed: bitvectors must be translated into lists
% so this is much slower than simJ
feature_pair_simGIC(F1,F2,Sim) :-
        feature_vector(F1,AV1),
        feature_vector(F2,AV2),
        AVI is AV1 /\ AV2,
        AVU is AV1 \/ AV2,
        % note that here we have to translate the bit vector back to
        % a list of attributes. maybe bitvectors don't but us much for
        % this metric? use prolog sets instead?
        vector_sumIC(AVI,ICI),
        vector_sumIC(AVU,ICU),
        Sim is ICI/ICU.

feature_pair_simICratio(F1,F2,Sim) :-
        feature_vector(F1,AV1),
        feature_vector(F2,AV2),
        AVI is AV1 /\ AV2,
        AVU is AV1 \/ AV2,
        % note that here we have to translate the bit vector back to
        % a list of attributes. maybe bitvectors don't but us much for
        % this metric? use prolog sets instead?
        vector_sumIC(AVI,ICI),
        Sim is ICI / popcount(AVU).


%% feature_pair_simNRGIC(?F1,?F2,-Sim:float)
% ∑ [a ∈ nr(A1 ∩ A2)] IC(a)  / ∑ [a ∈  nr(A1) ∪ nr(A2)∪ nr(A1 ∩ A2)]IC(t)
%
% as feature_pair_simGIC/3, taking the nonredundant nodes in the intersection set and each of the unique sets
% TODO
feature_pair_simNRGIC(F1,F2,Sim) :-
        feature_vector(F1,AV1),
        feature_vector(F2,AV2),
        vector_nr(AV1,AV1NR),   % TODO
        vector_nr(AV2,AV2NR),
        AVI is AV1 /\ AV2,
        vector_nr(AVI,AVINR),
        AVU is AV1NR \/ AV2NR \/ AVI,
        vector_sumIC(AVINR,ICI),
        vector_sumIC(AVU,ICU),
        Sim is ICI/ICU.

%% feature_pair_maxIC(?F1,?F2,-Max:float)
% Max[IC(a)] : a ∈ A1 ∩ A2
feature_pair_maxIC(F1,F2,MaxIC) :-
        feature_attributeset(F1,AL1),
        feature_attributeset(F2,AL2),
        ord_intersection(AL1,AL2,AL_Both),
        maplist(attribute_information_content,AL_Both,ICs),
        max_list(ICs,MaxIC).

/*
old___feature_pair_maxIC(F1,F2,MaxIC) :-
        feature_vector(F1,AV1),
        feature_vector(F2,AV2),
        AVI is AV1 /\ AV2,
        % note that here we have to translate the bit vector back to
        % a list of attributes. maybe bitvectors don't buy us much for
        % this metric? use prolog sets instead?
        vector_maxIC(AVI,MaxIC).
*/

%% feature_pair_maxIC_attributes(?F1,?F2,-Max:float,-Attributes:List)
% Max[IC(a)] : a ∈ A1 ∩ A2
feature_pair_maxIC_attributes(F1,F2,MaxIC,MaxAL) :-
        feature_attributeset(F1,AL1),
        feature_attributeset(F2,AL2),
        ord_intersection(AL1,AL2,AL_Both),
        maplist(attribute_information_content,AL_Both,ICs),
        max_list(ICs,MaxIC),
        list_pair_matches(AL_Both,ICs,MaxIC,MaxAL).

/*
old___feature_pair_maxIC_attributes(F1,F2,MaxIC,AL) :-
        feature_vector(F1,AV1),
        feature_vector(F2,AV2),
        AVI is AV1 /\ AV2,
        vector_maxIC_attributes(AVI,MaxIC,AL).
*/

%% feature_pair_maxIC_nr_attributes(+F1,+F2,?MaxIC,?AL_nr:list)
feature_pair_maxIC_nr_attributes(F1,F2,MaxIC,AL_nr) :-
        feature_pair_maxIC_attributes(F1,F2,MaxIC,AL),
        nr_subset(AL,AL_nr).

%% feature_pair_nr_ICatt_pairs(+F1,+F2,SumIC:float,?Pairs:list)
feature_pair_nr_ICatt_pairs(F1,F2,SumIC,SortedPairs) :-
        feature_attributeset(F1,AL1),
        feature_attributeset(F2,AL2),
        ord_intersection(AL1,AL2,AL_Both),
        nr_subset(AL_Both,AL_nr),
        setof(IC-A,
              (   member(A,AL_nr),
                  attribute_information_content(A,IC)),
              Pairs),
        reverse(Pairs,SortedPairs),
        findall(IC,member(IC-_,Pairs),ICs),
        sumlist(ICs,SumIC).

%% feature_pair_sumIC_ind_attributes(F1,F2,SumIC,SortedPairs)
% 
% calculate the sum of the ICs of the independent set of
% attributes shared by F1 and F2. The SumIC can be translated
% to a p-value.
%
% the set of independent attributes in common may include
% conjunctions of more than one attribute: e.g.
%
% [forelimb,hindlimb],[kidney],...
%
% here we treat forelimb and hindlimb as a conjunction, e.g.
% the phenotype co-manifests.
feature_pair_nr_independent_atts(F1,F2,SumIC,SortedPairs) :-
        feature_attributeset(F1,AL1),
        feature_attributeset(F2,AL2),
        ord_intersection(AL1,AL2,AL_Both),
        nr_subset(AL_Both,AL_nr),
        debug(sim,'  NR ~w',[AL_nr]),
        combine_correlated_attributes(AL_nr,AL_independent),
        debug(sim,'  NR_Indep: ~w',[AL_independent]),
        setof(IC-A,
              (   member(A,AL_independent),
                  attributeset_information_content(A,IC)),
              Pairs),
        reverse(Pairs,SortedPairs),
        findall(IC,member(IC-_,Pairs),ICs),
        sumlist(ICs,SumIC).

feature_pair_nr_independent_atts_corrected(F1,F2,N1,N2,SumIC,SortedPairs) :-
        feature_attributeset(F1,AL1),
        feature_attributeset(F2,AL2),
        attributeset_pair_csumIC_atts(AL1,AL2,N1,N2,SumIC,SortedPairs).

attributeset_pair_csumIC_atts(AL1,AL2,N1,N2,SumIC,SortedPairs) :-
        ord_intersection(AL1,AL2,AL_Both),
        AL_Both\=[],
        length(AL1,N1),
        length(AL2,N2),
        debug(sim,'  size: ~w',[N1*N2]),
        nr_subset(AL_Both,AL_nr),
        debug(sim,'  NR ~w',[AL_nr]),
        combine_correlated_attributes(AL_nr,AL_independent),
        debug(sim,'  NR_Indep: ~w',[AL_independent]),
        length(AL_independent,N3),
        CorrectionFactor is N3/(sqrt(N1) * sqrt(N2)),
        CorrectionOffset is -log(CorrectionFactor)/log(2),
        solutions(IC-A,
                  (   member(A,AL_independent),
                      attributeset_information_content(A,IC_Raw),
                      IC is IC_Raw - CorrectionOffset,
                                %debug(sim,'    A: ~w = ~w - ~w',[A,IC_Raw,CorrectionOffset]),
                      IC>0),
                  Pairs),
        reverse(Pairs,SortedPairs),
        findall(IC,member(IC-_,Pairs),ICs),
        sumlist(ICs,SumIC).

attributeset_information_content([A],IC) :-
        !,
        attribute_information_content(A,IC).
attributeset_information_content(Atts,IC) :-
        attributeset_feature_vector(Atts,FV),
        NumFA is popcount(FV),
        feature_count(NumF),
        IC is -(log(NumFA/NumF)/log(2)).

attributeset_feature_vector([A|Atts],V_Out) :-
        attribute_vector(A,FV),
        attributeset_feature_vector(Atts,FV,V_Out).

attributeset_feature_vector([],V,V).
attributeset_feature_vector([A|Atts],V_In,V_Out) :-
        attribute_vector(A,FV),
        V_Next is FV /\ V_In,
        attributeset_feature_vector(Atts,V_Next,V_Out).

        

% given a set of atts, a1, a2, ..., an
% make a list of conjunction sets (represented as lists),
% c1, .., cm where each
% set c is a set of correlated attributes [a1, ..], where
% each member of the list is correlated with one other
combine_correlated_attributes(Atts,ConjAtts) :-
        findall([A],member(A,Atts),ConjAttsSeed),
        iteratively_combine_correlated_attributes(ConjAttsSeed,ConjAtts).

iteratively_combine_correlated_attributes(Atts,AttsOut) :-
        debug(sim,'  combining ~w',[Atts]),
        % TODO: pick *best* pairing
        select(ASet1,Atts,Atts2),
        select(ASet2,Atts2,Atts3),
        combine_asets_if_correlated(ASet1,ASet2,ASet3),
        % new: make sure everything in group is correlated. greedy
        forall( (member(A1,ASet3), member(A2,ASet3), A1 @< A2),
                correlated_attribute_pair(A1,A2)),
        debug(sim,'   ~w + ~w ==> ~w',[ASet1,ASet2,ASet3]),
        !,
        iteratively_combine_correlated_attributes([ASet3|Atts3],AttsOut).
iteratively_combine_correlated_attributes(Atts,Atts) :- !.

combine_asets_if_correlated(Set1,Set2,SetJ) :-
        member(A1,Set1),
        member(A2,Set2),
        correlated_attribute_pair(A1,A2),
        append(Set1,Set2,SetJ).

% more split groups increases chance of groups being filtered when ICs for groups are corrected
correlated_attribute_pair(A1,A2) :-
        attribute_pair_pval_hyper(A1,A2,Vk,_Vn,_Vm,_VN,P),
        %debug(sim,'   ~w',[attribute_pair_pval_hyper(A1,A2,Vk,_Vn,_Vm,_VN,P)]),
        P < 0.000001, % HARCODE ALERT. TODO - use overlap as well
        Vk > 2.

/*
attribute_correction_factor(N) :-
        aggregate(count,A,attribute_used_at_least_twice(A),N).

attribute_used_at_least_twice(A) :-
        attribute_vector(A,V),
        N is popcount(V),
        N > 1.
*/
        
%% nr_subset(+AL_In:list,:AL_NR:lsit) is det
% true if AL_NR is the non-redundant subset of AL_In.
% requires attribute_subsumer/2
nr_subset(AL1,AL2) :-
        findall(A,
                (   member(A,AL1),
                    \+ ((member(A2,AL1),
                         A2\=A,
                         attribute_subsumer(A2,A),
                         \+ attribute_subsumer(A,A2)))),
                AL2).



%% feature_pair_cossim(?F1,?F2,-Sim:float)
% cosine similarity between two feature attributes
% vectors.
%
% ==
%           A1.A2
% acos  ------------
%       ||A1||.||A2||
% ==
% Since the angle, θ, is in the range of [0,π],
% the resulting similarity will yield the value of π as meaning exactly opposite,
% π / 2 meaning independent, 0 meaning exactly the same, with in-between values indicating intermediate similarities or dissimilarities.
feature_pair_cossim(F1,F2,Sim) :-
        feature_vector(F1,AV1),
        feature_vector(F2,AV2),
        AVP = AV1 /\ AV2,
        DP is popcount(AVP), % dot product
        M1 is popcount(AV1),
        M2 is popcount(AV2),
        Sim is acos(DP / (sqrt(M1)*sqrt(M2))).

feature_pair_avgICCSnr(F1,F2,Sim,AL_nr) :-
        feature_pair_avgICCS(F1,F2,Sim,AL),
        maplist(nr_subset,AL,AL_nr).

%% feature_pair_avgICCS(F1,F2,Sim)
feature_pair_avgICCS(F1,F2,Sim) :-
        feature_pair_avgICCS(F1,F2,Sim,_).

%% feature_pair_avgICCS(?F1,?F2,?AvgICCS:float,?AttributeSets:list)
% for each direct attribute, find the best match(es) in the other set.
% do this symmetrically for both F1 and F2
feature_pair_avgICCS(F1,F2,Sim,AL) :-
        feature_ix(F1,_),
        feature_ix(F2,_),
        setof(MaxIC-AMs,A^feature_pair_attribute_maxIC_set(F1,F2,A,MaxIC,AMs),Set1),
        setof(MaxIC-AMs,A^feature_pair_attribute_maxIC_set(F2,F1,A,MaxIC,AMs),Set2),
        union(Set1,Set2,Set),
        findall(MaxIC,member(MaxIC-_,Set),MaxICs),
        sumlist(MaxICs,MaxICSum),
        length(MaxICs,Len),
        Sim is MaxICSum/Len,
        findall(A,member(_-A,Set),AL).

% feature_pair_attribute_maxIC_set(?F1,?F2,?Att,-Max:float,?AttributesWithMax:list)
% for (+F1,+F2,+Att) this is deterministic.
% if Att is in A(F1) then find the best att A2(s) in A(F2) with its IC.
% there may be multiple such attributes, which is why we use a list
feature_pair_attribute_maxIC_set(F1,F2,A,MaxIC,AM) :-
        % TODO - fix this to use sets  and ord_intersection
        simmatrix:feature_attribute_direct(F1,A), % data unit-clause
        attribute_subsumer_vector(A,AV1), % derived from attribute_subsumer
        feature_vector(F2,AV2),
        AVI is AV1 /\ AV2,
        vector_maxIC_attributes(AVI,MaxIC,AM).

% NOTE: this makes more sense comparing attributes
%  by their features
feature_pair_pval_hyper(F1,F2,P) :-
        feature_pair_pval_hyper(F1,F2,_,_,_,_,P).
feature_pair_pval_hyper(F1,F2,Vk,Vn,Vm,VN,P) :-
        feature_vector(F1,AV1),
        feature_vector(F2,AV2),
        AV_Common = AV1 /\ AV2,
        Vk is popcount(AV_Common),
        Vn is popcount(AV1),
        Vm is popcount(AV2),
        attribute_count(VN),
        p_value_by_hypergeometric(Vk,Vn,Vm,VN,P).

feature_pair_all_scores(F1,F2,Scores) :-
        feature_ix(F1,_),
        feature_ix(F2,_),
        (   var(Scores)
        ->  default_scores(Scores)
        ;   true),        
        (   memberchk(simTO(SimTO),Scores)
        ->  feature_pair_ci(F1,F2,SimTO)
        ;   true),
        (   memberchk(simj(SimJ),Scores)
        ->  feature_pair_simj(F1,F2,SimJ)
        ;   true),
        (   memberchk(simGIC(SimGIC),Scores)
        ->  feature_pair_simGIC(F1,F2,SimGIC)
        ;   true),
        (   memberchk(simNRGIC(SimNRGIC),Scores)
        ->  feature_pair_simNRGIC(F1,F2,SimNRGIC)
        ;   true),
        (   memberchk(maxIC(MaxIC),Scores)
        ->  feature_pair_maxIC(F1,F2,MaxIC)
        ;   true),
        (   memberchk(maxIC(MaxIC,MaxICAttrs),Scores)
        ->  feature_pair_maxIC_attributes(F1,F2,MaxIC,MaxICAttrs)
        ;   true),
        (   memberchk(maxICnr(MaxIC,MaxICAttrs),Scores)
        ->  feature_pair_maxIC_nr_attributes(F1,F2,MaxIC,MaxICAttrs)
        ;   true),
        (   memberchk(avgICCS(ICCS,CSSL),Scores)
        ->  feature_pair_avgICCS(F1,F2,ICCS,CSSL)
        ;   true),
        (   memberchk(avgICCSnr(ICCS,CSSL),Scores)
        ->  feature_pair_avgICCSnr(F1,F2,ICCS,CSSL)
        ;   true),
        !.

default_scores([simTO(_),
                simj(_),
                simGIC(_),
                maxIC(_,_),
                avgICCS(_,_)]).


%% compare_feature_pair(+F1,+F2,-Score,+Opts)
% Opts :
%  metric(Metric) - one of simj, cossim, ci, or a list of Metrics. defaults to simj
compare_feature_pair(F1,F2,Score,Opts) :-
        (   member(metric(Metric),Opts)
        ->  true
        ;   Metric=simj),
        compare_feature_pair_by_metric(F1,F2,Score,Metric,Opts).

% in the case where multiple metrics are required, the score is of
% the form MainScore-[score(M1,S1),score(M2,S2),...].
% thus the result can still be used in a keysort
compare_feature_pair_by_metric(F1,F2,AllScore,L,Opts) :-
        L=[MainMetric|_],
        !,
        feature_ix(F1,_),
        feature_ix(F2,_),
        % this is inefficient as the same operations are performed
        % multiple times. TODO: optimize
        findall(score(Metric,Score),
                (   member(Metric,L),
                    compare_feature_pair_by_metric(F1,F2,Score,Metric,Opts)),
                Scores),
        member(score(MainMetric,MainScore),Scores),
        AllScore=MainScore*Scores.

compare_feature_pair_by_metric(F1,F2,MaxIC-MaxICAttrs,maxIC,_Opts) :-
        !,
        feature_pair_maxIC_attributes(F1,F2,MaxIC,MaxICAttrs).
compare_feature_pair_by_metric(F1,F2,Score,simj,_Opts) :-
        !,
        feature_pair_simj(F1,F2,Score).
compare_feature_pair_by_metric(F1,F2,Score,cossim,_Opts) :-
        !,
        feature_pair_cossim(F1,F2,Score).
compare_feature_pair_by_metric(F1,F2,Score,simGIC,_Opts) :-
        !,
        feature_pair_simGIC(F1,F2,Score).
compare_feature_pair_by_metric(F1,F2,Score,avgICCS,_Opts) :-
        !,
        feature_pair_avgICCS(F1,F2,Score).
compare_feature_pair_by_ci(F1,F2,Score,ci,_Opts) :-
        !,
        feature_pair_ci(F1,F2,Score).

%% feature_vector(?Feature:atom,-AttributeVector:int)
% relationship between a Feature and a vector of binary attribute values,
% encoded as a large int (the GMP library is required).
% The attribute vector has a bit set at position N (i.e. 2**N) if the feature has attribute N
% (where the mapping between the attribute and N is in attribute_ix/2).
% 
% this is calculated for a feature f as: ∑ [a ∈ fa(f)] 2**ix(a)
% (where ix is the index function and fa returns the set of attributes for a feature)
feature_vector(F,V) :-
        var(F),
        !,
        feature_ix(F,_),        % ground F then calculate
        feature_vector(F,V).
        
feature_vector(F,V) :-
        nonvar(F),
        feature_vector_cached(F,V),
        !.

feature_vector(F,V) :-
        template(F,Attribute,Goal),
        solutions(Num,(Goal,attribute_ix(Attribute,AI),Num is 2**AI),Nums),
        sumlist(Nums,V),
        assert(feature_vector_cached(F,V)).

feature_attributeset(F,AL) :-
        var(F),
        !,
        feature_exists(F),
        feature_attributeset(F,AL).
feature_attributeset(F,AL) :-
        nonvar(F),
        feature_attributeset_cached(F,AL),
        !.
feature_attributeset(F,AL) :-
        setof(A,feature_attribute(F,A),AL),
        assert(feature_attributeset_cached(F,AL)).


%% attribute_vector(?Attribute:atom,-AttributeVector:int)
%
% relationship between a Attribute and a vector of binary attribute values,
% representing features with this attribute,
% encoded as a large int (the GMP library is required).
%
% The attribute vector has a bit set at position N (i.e. 2**N) if the feature N has the attribute
% (where the mapping between the feature and N is in feature_ix/2).
% 
% this is calculated for a attribute a as: ∑ [f ∈ af(a)] 2**ix(f)
% (where ix is the index function and af returns the set of features for a attribute)
attribute_vector(A,V) :-
        var(A),
        !,
        attribute_ix(A,_),        % ground A then calculate
        attribute_vector(A,V).
        
attribute_vector(A,V) :-
        nonvar(A),
        attribute_vector_cached(A,V),
        !.

attribute_vector(A,V) :-        % A is ground, value is not yet cached
        template(Feature,A,Goal), % get features for this attribute
        % ∑ [f ∈ af(a)] 2**ix(f)
        solutions(Num,(Goal,feature_ix(Feature,AI),Num is 2**AI),Nums),
        sumlist(Nums,V),
        assert(attribute_vector_cached(A,V)).

%% attribute_pair_pval_hyper(+A1,+A2,?P:float)
%
% calculates correlation between two attributes based
% on shared features
attribute_pair_pval_hyper(A1,A2,P) :-
        attribute_pair_pval_hyper(A1,A2,_,_,_,_,P).
attribute_pair_pval_hyper(A1,A2,Vk,Vn,Vm,VN,P) :-
        attribute_vector(A1,FV1),
        attribute_vector(A2,FV2),
        FV_Common = FV1 /\ FV2,
        Vk is popcount(FV_Common),
        Vn is popcount(FV1),
        Vm is popcount(FV2),
        attribute_count(VN),
        p_value_by_hypergeometric(Vk,Vn,Vm,VN,P).


% attribute_subsumer_vector(+Att,-AttSubsumerVector:int)
% maps an attribute to an integer bitvector for all subsuming (parent) attributes
attribute_subsumer_vector(A,ASV) :-
        solutions(P,simmatrix:attribute_subsumer(A,P),Ps), % from data
        solutions(PI,(member(P,Ps),attribute_ix(P,PI)),PIs),
        solutions(Num,(member(PI,PIs),Num is 2**PI),Nums),
        sumlist(Nums,ASV),
        assert(attribute_subsumer_vector_cached(A,ASV)).

%% feature_avcount(?F:atom,-Num:int)
% true if F has Num attributes set
feature_avcount(F,Num) :-
        %feature_vector(F,V),
        %Num is popcount(V).
        feature_attributeset(F,AL),
        length(AL,Num).

% todo: use prolog sets?
vector_sumIC(AV,SumIC) :-
        vector_attributes(AV,AL),
        maplist(attribute_information_content,AL,ICs),
        sumlist(ICs,SumIC).

% given a vector of attributes, find the max IC of all attributes
vector_maxIC(AV,MaxIC) :-
        vector_attributes(AV,AL),
        maplist(attribute_information_content,AL,ICs),
        max_list(ICs,MaxIC).

% DEPRECATED
% vector_maxIC_attributes(+AV:int, ?MaxIC:float, ?MaxAL:list)
% as vector_maxIC/2, but include all attributes with IC matching MaxIC
vector_maxIC_attributes(AV,MaxIC,MaxAL) :-
        vector_attributes(AV,AL),
        maplist(attribute_information_content,AL,ICs),
        max_list(ICs,MaxIC),
        list_pair_matches(AL,ICs,MaxIC,MaxAL).

list_pair_matches([],[],_,[]).
list_pair_matches([A|AL],[MaxIC|ICs],MaxIC,[A|MaxAL]) :-
        !,
        list_pair_matches(AL,ICs,MaxIC,MaxAL).
list_pair_matches([_|AL],[_|ICs],MaxIC,MaxAL) :-
        !,
        list_pair_matches(AL,ICs,MaxIC,MaxAL).

        

%% vector_attributes(+AV:int,?AL:list)
% True if AV is an integer bit vector with the attributes in AL set
vector_attributes(AV,AL) :-
        vector_attributes(AV,AL,16).

vector_attributes(AV,AL,Window) :-
        Mask is 2**Window -1,
        vector_attributes(AV,ALx,0,Window,Mask),
        flatten(ALx,AL).

%% vector_attributes(+AV:int,?AL:list,+Pos,+Window,+Mask) is det
% Mask must = Window^2 -1 (not checked)
% shifts AV down Window bits at a time. If there are any bits in the window,
% use vector_attributes_lo/2 to get the attribute list from this window.
% note resulting list must be flattened.
% todo: difference list impl?
vector_attributes(0,[],_,_,_) :- !.
vector_attributes(AV,AL,Pos,Window,Mask) :-
        !,
        NextBit is AV /\ Mask,
        AVShift is AV >> Window,
        NextPos is Pos+Window,
        (   NextBit=0
        ->  vector_attributes(AVShift,AL,NextPos,Window,Mask)
        ;   vector_attributes_lo(NextBit,ALNew,Pos),
            AL=[ALNew|AL2],
            vector_attributes(AVShift,AL2,NextPos,Window,Mask)).
        
% as vector_attributes/2, but checks one bit at a time
vector_attributes_lo(AV,AL) :-
        vector_attributes_lo(AV,AL,0).

vector_attributes_lo(0,[],_) :- !.
vector_attributes_lo(AV,AL,Pos) :-
        NextBit is AV /\ 1,
        AVShift is AV >> 1,
        NextPos is Pos+1,
        (   NextBit=1
        ->  attribute_ix(Att,Pos),
            AL=[Att|AL2]
        ;   AL=AL2),
        !,
        vector_attributes_lo(AVShift,AL2,NextPos).

        

/** <module> statistics for feature-attribute matrices

  ---+ Synopsis

==
:- use_module(bio(simmatrix)).

likes(jim,cheese).
likes(jim,pizza).
likes(fred,pizza).
likes(fred,burrito).
likes(homer,beer).
likes(charlie,cheese).
% ...

% 
demo:-
  generate_term_indexes(F,A,likes(F,A)),
  feature_matches(jim,Matches,[limit(100)]),
  forall(member(Match,Matches),
         format('  Match: ~w~n',[Match])),
  feature_pair_simj(homer,charlie,Sim),
  writenl(Sim).
==

---+ Details

REQUIRES GMP!!

Given a predicate that relates a set of features F to a set of
attributes A, we can determine the similarity between any pair in F
based on the attributes they share in common.

This library uses bit vectors to store the attribute vector for each
feature (thus SWI-Prolog with GMP is required). The library user can
use arbitrary terms for the attributes, the library will take care of
mapping these to integers that index the bit vectors.


Use in the context of blip with GO data:

==
blip -r go_assoc_local/GeneDB_Tbrucei -u curation_db -u simmatrix -r go -u tabling
Starting blip shell
1 ?- table_pred(curation_db:curation_statementT/4).
true.

2 ?- generate_term_indexes(G,T,curation_statementT(_,G,_,T)).
true.

3 ?- feature_pair_simj('GeneDB_Tbrucei:Tb927.3.1380','GeneDB_Tbrucei:Tb10.389.1170',Num).
Num = 27.
==

This uses inferred annotations via ontol_reasoner:

==
blip -r implied/go -r go_assoc_local/GeneDB_Tbrucei -u curation_db -u ontol_db -u simmatrix -goal "generate_term_indexes(G,T,(curation_statement(_,G,_,T1),parent(T1,T)))"
==

---++ Recipes

Given a two-column file
==
load_biofile(tbl(likes),'file.tab'),
generate_term_indexes(P,F,likes(P,F)).
==

---++ Comparing attributes

Attributes may not be independent

---++ Performance notes

tested with feature set of size 170k, attribute set of size 28k, 13m positive attribute values

initial indexing takes ~1m

caching of all feature_vector/2 facts takes ~1gb

subsequent searches extremely fast

IC based metrics or any kind of weighting slows it down a lot...



---+ Additional Information

This module is part of blip. For more details, see http://www.blipkit.org

@author  Chris Mungall
@version $Revision$
@see     README
@license License


*/
