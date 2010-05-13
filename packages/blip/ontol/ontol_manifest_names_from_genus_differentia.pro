:- module(ontol_manifest_names_from_genus_differentia,[]).

:- use_module(bio(metadata_db)).
:- use_module(bio(ontol_db)).

metadata_db:entity_label(ID,N):-
        class(ID),
        genus(ID,Genus),
        findall(R=To,differentium(ID,R,To),Diffs),
        name_composed_type(Genus^Diffs,N).



