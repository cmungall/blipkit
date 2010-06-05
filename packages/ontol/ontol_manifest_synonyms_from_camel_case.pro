:- module(ontol_manifest_names_from_ids,[]).

% TODO - move this to metadata from ontol?  ... or keep for just classes only
:- use_module(bio(metadata_db)).

metadata_db:entity_synonym(ID,Syn):-
        entity_label(ID,N),
        split_on_camel_case(N,Syn).

split_on_camel_case(N,N2):-
        atom_chars(N,Chars),
        tokens_expanded(Chars,[' '|Chars2]),
        atom_chars(N2,Chars2).

tokens_expanded([],[]).
tokens_expanded([C|L],[' ',C|L2]):-
        C @>= 'A',
        C @=< 'Z',
        !,
        tokens_expanded(L,L2).
tokens_expanded([C|L],[C|L2]):-
        tokens_expanded(L,L2).

