:- module(stats_distributions,
          [log_factorial/2,
           logNCr/3,
           hypergeometric/5,
           p_value_by_hypergeometric/5
           ]).

:- use_module(bio(tabling)).
:- use_module(bio(bioprolog_util)).


log_factorial(0,0):- !.
log_factorial(1,0):- !.

%% log_factorial(+N,?R)
% 
log_factorial(N,R):-
        N>1,
        Nx is N-1,
        log_factorial(Nx,Rx),
        log(N,LogN),
        R is Rx+LogN,
        !.

:- arithmetic_function(log_factorial/1).
:- table_pred(log_factorial/2).

%% logNCr(+N,+R,?Result)
% @throws if N<R
logNCr(N,R,Result):-
        N>=R,
        !,
        Result is (log_factorial(N) - (log_factorial(R) +
                                      log_factorial(N-R))).
logNCr(N,R,_):-
        throw(logNCr(N<R)).

:- table_pred(logNCr/3).
:- arithmetic_function(logNCr/2).

%% hypergeometric(Vx,Vn,VM,VN,R)
%
%
% @param Vx: number in sample that have quality     (also: k)
% @param Vn: total in sample
% @param VM number in population that have quality (also: D)
% @param VN total population
hypergeometric(Vx,Vn,VM,VN,R):-
        R is exp(logNCr(VM,Vx) +
                logNCr(VN-VM,Vn-Vx) -
                logNCr(VN,Vn)).
:- arithmetic_function(hypergeometric/4).

%% p_value_by_hypergeometric(+Vx,+Vn,+VM,+VN,?P)
%
% Suppose that we have a total population of N genes,
% in which M have a particular annotation.
% If we observe x genes with that annotation,
% in a sample of n genes,
% then we can calculate the probability of that observation, using the hypergeometric distribution
%
% @param Vx: number in sample that have quality     (also: k)                        | k
% @param Vn: total in sample                                                         | n
% @param VM M: number in population that have quality (e.g. GO term) (also: D)       | m
% @param VN N: total population (e.g. total number of genes)                         | N
p_value_by_hypergeometric(Vx,Vn,VM,VN,P):-
        Min is min(VM,Vn),
        aggregate(sum(HG),
                  Vi,
                  (   between(Vx,Min,Vi),
                      hypergeometric(Vi,Vn,VM,VN,HG)),
                  P).
:- arithmetic_function(p_value_by_hypergeometric/4).



% -------------------- TESTS --------------------
% regression tests to ensure behaviour of module is correct;
% lines below here are not required for module functionality

unittest(test(lf,
             [],
             (   ensure_loaded(bio(stats_distributions)),
                 log_factorial(8,X),
                 X>10.6,
                 X<10.7,
                 log_factorial(8,Y), % 2nd time should use tabling
                 Y>10,
                 logNCr(4,3,Z),
                 format('logNCr(4,3)=~w~n',[Z]),
                 Z>1.3,
                 Z<1.4,
                 hypergeometric(4,10,5,50,RH),
                 format('hypergeometric(4,10,5,50)=~w~n',[RH]),
                 RH>0.0039,
                 RH<0.004
                 ),
            true)).

