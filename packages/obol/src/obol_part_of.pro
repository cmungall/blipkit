
:- use_module(bio(ontol_db)).

ontol_db:genus(X,G) :- subclass(X,G).
ontol_db:differentium(X,part_of,D) :- parent_over_nr(part_of,X,D).
