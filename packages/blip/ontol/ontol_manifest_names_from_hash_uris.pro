:- module(ontol_manifest_names_from_hash_uris,[]).

% TODO - move this to metadata from ontol?  ... or keep for just classes only
:- use_module(bio(metadata_db)).

metadata_db:entity_label(ID,N):-
        (   class(ID) ; property(ID)),
        concat_atom([_,N],'#',ID).


