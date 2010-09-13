:- module(ontol_concordance,
          [
           ontol_concordance_below/2,
           ontol_concordance_below/3
           ]).

:- use_module(bio(ontol_db)).
:- use_module(bio(stats_distributions)).
:- use_module(bio(bioprolog_util)).
:- use_module(bio(simmatrix)).
:- use_module(bio(index_util)).
:- use_module(bio(tabling)).
:- use_module(bio(blipkit)).

/*
ontol_concordance(C1,C2,Prob) :-
        ontol_concordance(C1,C2,_,_,_,Prob).

ontol_concordance(C1,C2,Set1,Set2,CommonL,Prob) :-
        solutions(I,C^inst_of(I,C),L),
        length(L,VN),
        debug(phenolog,'  total: N= ~w',[VN]),
        ontol_concordance(C1,C2,VN,Set1,Set2,CommonL,Prob).

ontol_concordance(C1,C2,VN,Set1,Set2,CommonL,Prob) :-
        solutions(I,inst_ofRT(I,_,C1),Set1),
        length(Set1,Vn),
        debug(phenolog,'  total instances with attribute: ~w (set1): n= ~w',[C1,Vn]),

        solutions(I,inst_ofRT(I,_,C2),Set2),
        length(Set2,Vm),
        debug(phenolog,'  total instances with attribute: ~w (set2): m= ~w',[C2,Vm]),

        ord_intersection(Set1,Set2,CommonL),
        length(CommonL,Vk),
        debug(phenolog,'  total instances with both: k= ~w // ~w',[Vk,CommonL]),

        debug(phenolog,'CALC: ~w',hypergeometric(Vk,Vn,Vm,VN,Prob)),
        p_value_by_hypergeometric(Vk,Vn,Vm,VN,Prob).
*/


run_ontol_concordance(C1,_,5,ExL) :-
        member(C1,ExL),         % exclude
        !.
run_ontol_concordance(_,C2,5,ExL) :-
        member(C2,ExL),         % exclude
        !.
run_ontol_concordance(C1,C2,Vk,_) :-
        !,
        feature_pair_pval_hyper(C1,C2,Vk,Vn,Vm,VN,P),
        debug(phenolog,'  R:~w',[ontol_concordance(C1,C2,Vk,Vn,Vm,VN,P)]),
        (   Vk >= 1
        ->  blipkit:show_factrow([isLabel(1),
                                  isUseTabs(1)],
                                 ontol_concordance(C1,C2,Vk,Vn,Vm,VN,P))
        ;   true).

prepare(C1,C2) :-
        table_pred(ontol_db:parentT/3),
        materialize_index(ontol_db:inst_ofRT(1,0,1)),
        generate_term_indexes(C,I,inst_ofRT(I,_,C)).

/*
        % population size is number of instances that are in both
        aggregate(count,I,(inst(I),
                           \+ \+ inst_ofRT(I,_,C1),
                           \+ \+ inst_ofRT(I,_,C2)),
                  NumI),
        nb_setval(attribute_count,NumI).
*/

ontol_concordance_below(C1,C2,ExL) :-
        prepare(C1,C2),
        iter([C1-C2],[C1,C2],[],ExL).

ontol_concordance_below(C1,C2) :-
        prepare,
        iter([C1-C2],[],[C1,C2]).

iter([],_,_).
iter([C1-C2|Pairs],VisL,ExL) :-
        member(C1-C2,VisL),
        !,
        iter(Pairs,VisL,ExL).
iter([C1-C2|Pairs],VisL,ExL) :-
        !,
        debug(phenolog,'  pair: ~w',[C1-C2]),
        run_ontol_concordance(C1,C2,Num,ExL),
        ext(C1,C2,Num,NextPairs,ExL),
        sort(NextPairs,NextSet),
        ord_union(NextSet,Pairs,Pairs2),
        iter(Pairs2,[C1-C2|VisL],ExL).


ext(_,_,Num,[],_) :- Num < 2,!.
ext(C1,C2,_,NextPairs,ExL) :-
        % cross-product of children
        findall(X1,(parent(X1,C1),class(X1)),X1s),
        findall(X2,(parent(X2,C2),class(X2)),X2s),
        findall(X1-X2,((member(X1,[C1|X1s]),
                        member(X2,[C2|X2s]),
                        X1-X2 \= C1-C2),
                       feature_exists(X1),
                       feature_exists(X2),
                       \+member(X1,ExL),
                       \+member(X2,ExL)
                      ),
                NextPairs).

        
