:- module(pairwise_concordance,
          [
           pairwise_concordance/4,
           pairwise_concordance/7
           ]).


%% pairwise_concordance(FSet1,FSet2,Pairs,PVal)
% FSet1 = features in set1 with attribute
% FSet2 = features in set2 with attribute
% Pairs = [F-F,F-F,...]
pairwise_concordance(FSet1,FSet2,Pairs,Prob) :-
        pairwise_concordance(FSet1,FSet2,Pairs,Prob,_,_,_).
pairwise_concordance(FSet1,FSet2,Pairs,Prob,PairsS1,PairsS2,CommonPairs) :-
        length(Pairs,VN),
        debug(phenolog,'  total pairs: N= ~w',[VN]),

        solutions(G1-G2,(member(G1,FSet1),
                         member(G1-G2,Pairs)),
                  PairsS1),
        length(PairsS1,Vn),
        debug(phenolog,'  total pairs with phenotype (set1): n= ~w',[Vn]),

        solutions(G1-G2,(member(G2,FSet2),
                         member(G1-G2,Pairs)),
                  PairsS2),
        length(PairsS2,Vm),
        debug(phenolog,'  total pairs with phenotype (set2): m= ~w',[Vm]),

        solutions(G1-G2,(member(G1,FSet1),
                         member(G1-G2,Pairs),
                         member(G2,FSet2)),
                  CommonPairs),
        length(CommonPairs,Vk),
        debug(phenolog,'  total shared pairs with phenotype: k= ~w // ~w',[Vk,CommonPairs]),

        debug(phenolog,'CALC: ~w',hypergeometric(Vk,Vn,Vm,VN,Prob)),
        p_value_by_hypergeometric(Vk,Vn,Vm,VN,Prob).
