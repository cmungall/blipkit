:- module(homol_manifest_ontol_hset,[]).
:- use_module(bio(homol_db)).
:- use_module(bio(ontol_db)).
:- use_module(bio(homol_bridge_from_ontol)).

ontol_db:restriction(X,member_of,SA) :-
	class(X),
	homologous_to_set(X,S),
	concat_atom(S,'-',SA).

%ontol_db:class(SA) :-
%	setof(SA,X^restriction(X,member_of_hset,SA),SAL),
%	member(SA,SAL).

