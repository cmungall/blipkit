
:- use_module(bio(ontol_db)).

ontol_db:genus(X,G) :- subclass(X,G).
%ontol_db:differentium(X,has_part,D) :- parent_over(part_of,D,X).
ontol_db:differentium(X,has_part,D) :- parent_over_nr(part_of,D,X).
