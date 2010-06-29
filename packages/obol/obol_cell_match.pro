:- use_module(bio(ontol_db)).
:- use_module(bio(bioprolog_util)).

stemmy('CL:0000034'). % stem cell
stemmy('CL:0000055'). % non-terminally differentiated cell

xp(X,G,[develops_into=Y]):-
        stemmy(G),
        subclassT(X,G),
        solutions(Desc,restriction(Desc,develops_from,X),Descs),
        subclass_lca(Descs,Y).

        