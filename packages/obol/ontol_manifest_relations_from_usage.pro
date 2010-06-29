/* -*- Mode: Prolog -*- */
:- module(ontol_manifest_relations_from_usage,
          []).

:- use_module(bio(bioprolog_util)).
:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).

unasserted_relation(R):-  setof(R,unasserted_relation1(R),Rs),member(R,Rs).
unasserted_relation1(R):-  restriction(_,R,_),\+entity_label(R,_).
unasserted_relation1(R):-  differentium(_,R,_),\+entity_label(R,_).

ontol_db:property(R):- unasserted_relation(R).
metadata_db:entity_resource(R,unassigned_relation):- unasserted_relation(R).
metadata_db:entity_comment(R,Cmt):-
        unasserted_relation(R),
        setof_count(X,differentium(X,R,_),Num),
        sformat(Cmt,'needs to be defined. Used in ~w xp defs',[Num]).
metadata_db:entity_xref(R,X):-
        property(X,R).


        
