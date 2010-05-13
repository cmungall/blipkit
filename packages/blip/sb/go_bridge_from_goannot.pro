
has_location(G,X) :- curation_statement(_,G,_,X),belongs(X,cellular_component).

has_unique_nr_location(G,X) :-
        has_location(G,X),
        debug(loc,'testing ~w ~w',[G,X]),
        \+ ((has_location(G,Y),
             X\=Y,
             \+ parentT(X,Y),
             debug(loc,'  counter ~w ~w',[G,Y]))).
