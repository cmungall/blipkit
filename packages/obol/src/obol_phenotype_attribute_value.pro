:- multifile phenotype/3.

% high brain weight
phenotype(Q that inheres_in(B)) --> [V],terminal(B),[A],{av2q(A,V,Q)}.

% increased cellular sensitivity to X
phenotype(Q that towards(E) and inheres_in(B)) --> [V],anatomy_relational_adj(B),[A],[to],terminal(E),{rav2q(A,V,Q)}.

% eg increased+level = increased level REDUNDANT with above
av2q(A,V,Q):-
        (   nonvar(A),
            nonvar(V)
        ->  concat_atom([A,V],' ',QN),
            class_label(Q,QN,_)
        ;   class_label(Q,QN,_),
            concat_atom([A,V],' ',QN)).
rav2q(A,V,Q):-
        (   nonvar(A),
            nonvar(V)
        ->  concat_atom([A,V,towards],' ',QN),
            class_label(Q,QN,_)
        ;   class_label(Q,QN,_),
            concat_atom([A,V,towards],' ',QN)).

