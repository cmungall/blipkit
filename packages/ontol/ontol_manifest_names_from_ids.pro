:- module(ontol_manifest_names_from_ids,[]).
:- use_module(bio(ontol_db)).


% TODO - move this to metadata from ontol?  ... or keep for just classes only
:- use_module(bio(metadata_db)).

metadata_db:entity_label(ID,N):-
        (   class(ID)
        ;   property(ID)
        ;   inst(ID)),
        \+is_anonymous(ID),
        (   concat_atom([_,N],'#',ID)
        ->  true
        ;   concat_atom([_,N],':',ID)
        ->  true
        ;   N=ID).
