:- module(ontol_manifest_names_from_camelcase_ids,[]).
:- use_module(bio(ontol_db)).


% TODO - move this to metadata from ontol?  ... or keep for just classes only
:- use_module(bio(metadata_db)).

metadata_db:entity_label(ID,N):-
        class(ID),
        \+is_anonymous(ID),
        concat_atom([_,N1],'#',ID),
        split_on_camel_case(N1,N).

metadata_db:entity_label(ID,N):-
        class(ID),
        \+is_anonymous(ID),
        concat_atom([_,N1],':',ID),
        split_on_camel_case(N1,N).

split_on_camel_case(N,N2dc):-
        atom_chars(N,Chars),
        tokens_expanded(Chars,[' '|Chars2]),
        atom_chars(N2,Chars2),
        downcase_atom(N2,N2dc).


tokens_expanded([],[]).
tokens_expanded([C|L],[' ',C|L2]):-
        C @>= 'A',
        C @=< 'Z',
        !,
        tokens_expanded(L,L2).
tokens_expanded([C|L],[C|L2]):-
        tokens_expanded(L,L2).


