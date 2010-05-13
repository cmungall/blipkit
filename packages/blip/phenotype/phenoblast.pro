:- module(phenoblast,
         [
	  calculate_method_feature_pair_phenosim/4
	  ]).

:- use_module(phenotype_db).
:- use_module(pkb_db).
:- use_module(bio(bioprolog_util),[solutions/3]).
:- use_module(bio(ontol_db)).
:- use_module(bio(tabling)).

:- multifile feature_pair_simnr/4. % to be deprecated?
:- multifile feature_pair_simj/3.
:- multifile feature_pair_simj_all/4.
:- multifile feature_pair_maxIC/4.
:- multifile feature_phenotype_pair_simj/6.
:- multifile feature_phenotype_pair_simj_all/4.



method_feature_phenoprofile(precomposed,F,Ps) :-
	setof(P,(organism_phenotype(F,P),
		 atom(P)),
	      Ps).
method_feature_phenoprofile(postcomposed,F,Ps) :-
	setof(P,(feature_phenotype(F,P),
		 \+atom(P)),Ps).
%	setof(PQ,(organism_phenotype(F,P),
%		  phenotype_quad(P,PQ)),Ps).

phenotype_db:method_feature_pair_phenosim(M,F1,F2,Results) :-
	phenotype_db:feature_pair_simnr(M,F1,F2,Results).
phenotype_db:method_feature_pair_phenosim(simj_all,F1,F2,[simj(S),subsumers(L)]) :-
	feature_pair_simj_all(F1,F2,S,L).
phenotype_db:method_feature_pair_phenosim(max_ic,F1,F2,[max_ic(S,L)]) :-
	feature_pair_maxIC(F1,F2,S,L).
phenotype_db:method_feature_pair_phenosim(psimj,F1,F2,Results) :-
	setof(P1-P2-S,MaxIC^feature_phenotype_pair_simj(F1,F2,P1,P2,S,MaxIC),Pairs), % todo use maxic
	setof(P1-P2-S,
	      (	  member(P1-P2-S,Pairs),
		  \+ ((member(P1-_-SB,Pairs),
		       SB>S))),
	      BestPairs1),
	setof(P1-P2-S,
	      (	  member(P1-P2-S,Pairs),
		  \+ ((member(_-P2-SB,Pairs),
		       SB>S))),
	      BestPairs2),
	append(BestPairs1,BestPairs2,BestPairsBoth),
	findall(S,member(_-_-S,BestPairsBoth),SL),
	sumlist(SL,Sum),
	length(SL,LenSL),
	Avg is Sum/LenSL,
				% not really IC...
	Results=[bestmatches(BestPairs1,BestPairs2),avg_ic(Avg)].



calculate_method_feature_pair_phenosim(Method,F1,F2,Results) :-
        debug(phenoblast,'comparing: ~w vs ~w',[F1,F2]),

	% first extract phenotype profiles P1s and P2s of pair
	method_feature_phenoprofile(Method,F1,P1s),
	method_feature_phenoprofile(Method,F2,P2s),
        debug(phenoblast,'~w p1s = ~q',[Method,P1s]),
        debug(phenoblast,'~w p2s = ~q',[Method,P2s]),

	% all possible pairings P1s x P2s
        setof(P1-P2,(member(P1,P1s),member(P2,P2s)),PPairs),
        length(PPairs,NumPPairs),
        debug(phenoblast,'num pairwise combos = ~w',[NumPPairs]),
	%NumPPairs < 4000,   % arbitrary, too many takes too long. TODO
	
        % for each asserted phenotype, find the best match in the opposite set
        findall(P1-PX-PSub-IC,(member(P1,P1s),solutions(PX,member(P1-PX,PPairs),PXs),phenotype_candidates_bestmatch(Method,P1,PXs,PX,PSub,IC)),
		P1Xs),
        debug(phenoblast,'phenotype-bestmaches set[1]=~w',[P1Xs]),

        findall(P2-PX-PSub-IC,(member(P2,P2s),solutions(PX,member(PX-P2,PPairs),PXs),phenotype_candidates_bestmatch(Method,P2,PXs,PX,PSub,IC)),
		P2Xs),
        debug(phenoblast,'phenotype-bestmaches set[2]=~w',[P2Xs]),
	
        % now find all unique subsumers (with their ICs)
        setof(PSub-IC,P^PX^(   member(P-PX-PSub-IC,P1Xs)
			   ;   member(P-PX-PSub-IC,P2Xs)),
              PSubICPairs),
        debug(phenoblast,'unique phenotype subsumers and their ICs=~w',[PSubICPairs]),

	% we compute two statistics: max_ic and the average ic of the minimal set of subsumers.
        aggregate(sum(IC),PSub,member(PSub-IC,PSubICPairs),TotalIC),
        aggregate(max(IC),PSub,member(PSub-IC,PSubICPairs),MaxIC),
        aggregate(count,PSub,IC^member(PSub-IC,PSubICPairs),Total),
        debug(phenoblast,'scores=~w ~w',[TotalIC/Total,MaxIC]),
        ICCS is TotalIC/Total,

	Results = [bestmatches(P1Xs,P2Xs),
		   max_ic(MaxIC),
		   avg_ic_bestmatches(ICCS)].

%% phenotype_candidates_bestmatch(+Method,+Phenotype,+CandidatePhenotypes,?Bestmatch,?Subsumer,?IC)
% given a phenotype from set1 and all phenotypes from set2, find the best match from set2, and the subsumer.
% there are two versions, depending on Method = precomposed | postcomposed
phenotype_candidates_bestmatch(precomposed,P1,PXs,PX,PSub,IC) :-
        debug(phenoblast,'   finding phenotype_candidates_bestmatch for: ~q',[P1]),
        setof(Freq-PX-PSub,(member(PX,PXs),class_pair_prereasoned_lca(P1,PX,PSub),precomposed_phenotype_frequency(PSub,Freq)),
	      [Freq-PX-PSub|_]),
        debug(phenoblast,'   phenotype_candidates_bestmatch: ~q',[PX]),
        freq_ic(Freq,IC).
phenotype_candidates_bestmatch(postcomposed,P1,PXs,PX,PSub,IC) :-
        debug(phenoblast,'   finding phenotype_candidates_bestmatch for: ~q',[P1]),
        setof(Freq-PX-PSub,(member(PX,PXs),phenotype_lca(P1,PX,PSub),phenotype_frequency(PSub,Freq)),
	      [Freq-PX-PSub|_]),
        freq_ic(Freq,IC).

class_pair_prereasoned_ca(P1,PX,PSub) :-
	debug(phenoblast,'finding common ancestor of ~w -vs- ~w',[P1,PX]),
	subclass(P1,PSub),
	subclass(PX,PSub).
:- table_pred(class_pair_prereasoned_ca/3).


class_pair_prereasoned_lca(P1,PX,PSub) :-
	class_pair_prereasoned_ca(P1,PX,PSub),
	\+ ((class_pair_prereasoned_ca(P1,PX,PSub2),
	     PSub2\=PSub,
	     subclass(PSub2,PSub))).

precomposed_phenotype_frequency(P,Num) :-
        aggregate(count,F,inferred_feature_precomposed_phenotype(F,P),Num),
        debug(phenoblast,'freq(~w) =~w',[P,Num]).
:- table_pred(precomposed_phenotype_frequency/2).

inferred_feature_precomposed_phenotype(F,P) :-
	subclass(P1,P),		% pre-reasoned
	organism_phenotype(F,P1).

freq_ic(Freq,IC) :-
        feature_count(Num),
        PVal is Freq/Num,
        IC is -log(PVal)/log(2).

/** <module> computes similarity between features based on phenotypes

Here 'feature' is any phenotype-associated entity - organism, genotype, gene, disease etc.

using this module will cause method_feature_pair_phenosim/3 in the phenotype_db module to be dynamically calculated

uses phenotype_lca/3 in phenotype_db

  
  
*/
