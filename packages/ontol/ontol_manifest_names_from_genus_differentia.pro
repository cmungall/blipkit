:- module(ontol_manifest_names_from_genus_differentia,[]).

:- use_module(bio(metadata_db)).
:- use_module(bio(ontol_db)).

metadata_db:entity_label(ID,N):-
        class(ID),
        genus(ID,Genus),
        findall(R=To,differentium(ID,R,To),Diffs),
        name_composed_type(Genus^Diffs,N).


name_composed_type(Genus^Diff,Name):-
        !,
        name_composed_type(Genus,GenusName),
        name_composed_type(Diff,Which),
        concat_atom([GenusName,' that ',Which],Name).
name_composed_type(Rel=To,N):-
        !,
        name_composed_type(Rel,RelN),
        name_composed_type(To,ToN),
        concat_atom([RelN,' ',ToN],N).
name_composed_type([],''):- !.
name_composed_type([D],N):- !, name_composed_type(D,N).
name_composed_type([D|Ds],N):-
        !,
        name_composed_type(D,N1),
        name_composed_type(Ds,N2),
        concat_atom([N1,' and ',N2],N).
name_composed_type(C,N):- class(C,N),!.
name_composed_type(C,N):- property(C,N),!.
name_composed_type(X,X).



