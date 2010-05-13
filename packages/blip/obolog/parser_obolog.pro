/* -*- Mode: Prolog -*- */

/*
  maps IDs
*/

:- module(parser_obolog,
          [
          ]).
:- use_module(bio(obolog_db)).
:- use_module(bio(kif2cl)).
:- use_module(bio(sxpr_parser),
	      [
	       stream_sxprs/2,
	       sxpr_prolog/2
	      ]).
:- use_module(bio(bioprolog_util)).
:- use_module(bio(io)).

:- multifile io:parse_stream/2.

io:parse_stream(obolog,IO):-
        stream_sxprs(IO,Xs),
        maplist(sxpr_prolog,Xs,Facts),
        debug(obolog,'facts: ~w',[Facts]),
        filter_facts(Facts,FilteredFacts,Facts),
        debug(obolog,'now mapping IDs',[]),
        % invert
        solutions(formal_label(ID,Label),(member(exported_identifier(Label,ID),FilteredFacts),ID\=Label),LabelFacts),
        map_identifiers(FilteredFacts,IDMappedFacts),
        append(LabelFacts,IDMappedFacts,AllFacts),
        debug(obolog,'mapped facts:',[]),
        maplist(obolog_db:assert_formula,AllFacts).
        %maplist(obolog_db:assert,AllFacts).

% no mappings of IDs
io:parse_stream(obolog_simple,IO):-
        stream_sxprs(IO,Xs),
        maplist(sxpr_prolog,Xs,Facts),
        filter_facts(Facts,FilteredFacts,Facts),
        debug(obolog,'now mapping IDs',[]),
        debug(obolog,'mapped facts:',[]),
        maplist(obolog_db:assert_formula,FilteredFacts).
        %maplist(obolog_db:assert,FilteredFacts).

% human readable
io:parse_stream(obolog_hr,IO):-
        debug(obolog,'Stream: ~w',[IO]),
        stream_sxprs(IO,Xs),
        maplist(sxpr_prolog,Xs,Facts),
        filter_facts(Facts,FilteredFacts,Facts),
        debug(obolog,'now mapping IDs',[]),
        findall(ID->Label,member(label(ID,Label),Facts),Mappings),
        map_identifiers(FilteredFacts,IDMappedFacts,Mappings),
        debug(obolog,'mapped facts:',[]),
        maplist(obolog_db:assert_formula,IDMappedFacts).
        %maplist(obolog_db:assert,IDMappedFacts).

translate_facts(Facts,TFacts,AllFacts) :-
        maplist(cl_to_kif,Facts,CLFacts), % TODO
        filter_facts(CLFacts,TFacts,AllFacts).


%% filter_facts(+Facts,?FilteredFacts,+AllFacts)
% special purpose processing. handles comments
filter_facts([],[],_):- !.
filter_facts([not(T)|L],L2,Facts):-
        !,
        filter_facts([negated(T)|L],L2,Facts).
filter_facts([$comment(CA)|L],L2,Facts):- % start of structured comments
	atom_concat('; @',C,CA),
        !,
        filter_facts([$sc(C)|L],L2,Facts).
filter_facts([$sc(C),$comment(CA)|L],L2,Facts):- % join sc with succeeding comments
        !,
	atom_concat(C,CA,C2),
        filter_facts([$sc(C2)|L],L2,Facts).
filter_facts([$sc(C),H|L],[$sc(C,H)|L2],Facts):- % join sc with sentence
        !,
        filter_facts(L,L2,Facts).
filter_facts([$comment(_)|L],L2,Facts):- % filter non-structured comments
        !,
        filter_facts(L,L2,Facts).
filter_facts([H|L],[module(Name)|L3],Facts):-
	H =.. ['cl:module',Name|Phrases],
        !,
        filter_facts(Phrases,L1,Facts),
	findall(MF,(member(F,L1),MF=module(Name,F)),L1M),
        filter_facts(L,L2,Facts),
	append(L1M,L2,L3).
filter_facts([T|L],L2,Facts):-
        filter_fact(T,Facts),
        !,
        filter_facts(L,L2,Facts).
filter_facts([T|L],[T|L2],Facts):-
        filter_facts(L,L2,Facts).


