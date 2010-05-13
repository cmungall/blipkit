/* -*- Mode: Prolog -*- */

:- module(ontol_writer_powerloom, [
                             write_ontology/2,
                             write_header/1,
                             write_class/2,
                             write_property/2
                             ]).

:- use_module(bio(mode)).
:- use_module(bio(ontol_db)).
:- use_module(bio(bioprolog_util)).

io:redirect_stdout(powerloom).

io:write_all(powerloom,_,_):-
        write_header(powerloom),
        %dsetof(O,ID^N^(belongs(ID,O),class(ID,N)),Os),
        % collect all ontology IDs first
        dsetof(O,ID^belongs(ID,O),Os),
        % check for inclusion/exclusion specified by user
        forall((member(O,Os),io:in_include_list(O,ontology)),
               write_ontology(powerloom,O)),
        forall_distinct(inst_of(ID,_),
                        write_inst(powerloom,ID)).

write_header(powerloom):-
        wsxpr(defmodule(dq(obo),':includes',dq('PL-USER'))),
        wsxpr(['in-module',dq(obo)]),
        wsxpr(defconcept(entity)),
        wsxpr(defrelation(name,[['?x',entity],['?n','STRING']])),
        nl.

:- mode write_ontology(+,+) is det.
write_ontology(powerloom,O):-
        forall_distinct(belongs(ID,O),
                        write_entity(powerloom,ID)).

write_entity(powerloom,Class):-
        class(Class,_),
        write_class(powerloom,Class).
write_entity(powerloom,Class):-
        property(Class,_),
        write_property(powerloom,Class).

write_class(powerloom,Class):-
        class(Class,Name),
        wsxpr(defconcept(Class)),
        wsxpr(assert(['name',Class,dq(Name)])),
        forall(subclass(Class,Parent),wsxpr(assert(['subset-of',Class,Parent]))),
        forall(restriction(Class,Relation,ToClass),
               (   wsxpr(assert(['=>',
                                 [Class,'?x'],
                                 exists(['?y'],
                                        and([ToClass,'?y'],
                                            [Relation,'?x','?y']))])),
                   wsxpr(assert(['holds-all-some',Relation,Class,ToClass])))),
        nl.

write_property(powerloom,Rel):-
        property(Rel,Name),
        (property_domain(Rel,Domain)->true;Domain=entity),
        (property_range(Rel,Range)->true;Range=entity),
        wsxpr(defrelation(Rel,[['?x',Domain],['?y',Range]])),
        wsxpr(assert(['name',Rel,dq(Name)])),
        forall(subclass(Rel,Parent),wsxpr(assert(['subset-of',Rel,Parent]))),
        forall(is_transitive(Rel),wsxpr(assert(['transitive',Rel]))),
        nl.

        
wsxpr(Term):-
        sxpr(Term,Tokens,[]),
        maplist(write,Tokens).

sxpr(_-dq(X)) --> !,['"',X,'" '].
sxpr(D-L) --> {is_list(L),!,D2 is D+1},['('],sxpr_list(D2-L),[')','\n'].
sxpr(D-T) --> {compound(T),!,T=..L},sxpr(D-L).
sxpr(_-X) --> !,[X,' '].
sxpr(X)   --> sxpr(0-X).
sxpr_list(D-[H|T]) --> !,sxpr(D-H),sxpr_list(D-T).
sxpr_list(_-[]) --> [].
