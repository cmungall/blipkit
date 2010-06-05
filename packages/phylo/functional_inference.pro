
% phylogenetic inference
descended_from(P,C) :- descended_from(P,Z), descended_from(Z,C).
nad(X,Y) :- \+ descended_from(X,Y), \+ descended_from(Y,X).

% G has_function F if a curator says so (with evidence)
has_function(G,F) :- asserted_has_function(G,F).

% inference using GO
has_function(G,F) :- is_a(F2,F),has_function(G,F2).
has_function(G,F) :- part_of(F2,F),has_function(G,F2).

has_function(G,F) :- \+ in_organism_lacking_function(G,F).

% G lacks_function F if a curator says so
lacks_function(G,F) :- asserted_lacks_function(G,F).

% negation causes downward propagation
lacks_function(G,F) :- is_a(F,F2),lacks_function(G,F2).
lacks_function(G,F) :- part_of(F,F2),lacks_function(G,F2).

% propagation DOWN the phylogenetic tree:
% functions propagate down unless there was an ancestral loss
has_function(G,F) :- descended_from(G,G2),has_function(G2,F), \+ lacks_function(G2,F).

% suggest annotations for leaf nodes
suggested_has_function_annotation(G,F) :- has_function(G,F), \+ asserted_has_function(G,F), leaf_node(G).

% propagation UP the phylogenetic tree:
% here we would ideally have a probabilistic model.
% we assume potential for annotation 
has_function(G,F) v descendents_independently_evolved(G,F) :-
        has_function(GC1,F),descended_from_by_speciation(GC1,G),
        has_function(GC2,F),descended_from_by_speciation(GC2,G),
        nad(GC1,GC2).


% NEGATED propagation UP the phylogenetic tree:
lacks_function(G,F) v descendants_independently_lost(G,F) :-
        lacks_function(GC1,F),descended_from_by_speciation(GC1,G),
        lacks_function(GC2,F),descended_from_by_speciation(GC2,G),
        nad(GC1,GC2),
        \+ in_organism_lacking_function(G,F).








