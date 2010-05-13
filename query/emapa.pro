:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).
:- use_module(bio(tabling)).

metadata_db:entity_resource(G,emapax) :- cg(_,G,_).
ontol_db:class(G) :- cg(_,G,_).

cg(X,G,N) :-
        entity_label(X,N),
        \+ \+ restriction(X,part_of,_),
        \+ \+ ((entity_label(Y,N),
                Y\=X)),
        atom_concat('EMAPA:',N,G).

:- table_pred(cg/3).

ontol_db:genus(X,G) :-
        cg(X,G,_).

ontol_db:differentium(X,part_of,Y) :-
        genus(X,_),
        restriction(X,part_of,Y).


