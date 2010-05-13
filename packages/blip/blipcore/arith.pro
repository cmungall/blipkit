:- arithmetic_function(count_distinct/2).
count_distinct(Var,Goal,Count):-
        setof_count(Var,Goal,C1),
        Count is C1+0.
