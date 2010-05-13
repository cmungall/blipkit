/* -*- Mode: Prolog -*- */

:- module(web_fetch_cancergrid,[
                                web_search_cancergrid/3,
                                web_search_cancergrid/4
                               ]).

:- use_module(bio(web_fetch)).
:- use_module(bio(xml_transform)).
:- use_module(library('http/http_client')).
:- use_module(library(sgml_write)).

url('http://canmed-onc3469.medsch.ucl.ac.uk:8080/?E=').

web_fetch:search_term_to_url(cancergrid,findall(QueryTemplate,Goal),URLFull,_):-
        url(URL),
        sformat(QueryTemplateAtom,'~q',[QueryTemplate]),
        sformat(GoalAtom,'~q',[Goal]),
        E=element('env:Envelope',
                  ['xmlns:env'='http://www.w3.org/2003/05/soap-envelope'],
                  [element('env:Body',
                           [],
                           [element('Prolog',
                                    [template=QueryTemplateAtom,
                                     goal=GoalAtom],
                                    [])])]),
        call_on_stream(S,sgml_write(S,E,[]),XMLAtom),
        debug(web,'enc=~w',[XMLAtom]),
        www_form_encode(XMLAtom,XMLAtomEncoded),
        debug(web,'enc=~w',[XMLAtomEncoded]),
        atom_concat(URL,XMLAtomEncoded,URLFull).

%% web_search_cancergrid(+Template,+Goal,?Results) is det
web_search_cancergrid(Template,Goal,Results):-
        web_search_cancergrid(Template,Goal,Results,[]).
%% web_search_cancergrid(+Template,+Goal,?Results,+Opts) is det
web_search_cancergrid(Template,Goal,Items,Opts):-
        web_fetch(cancergrid,findall(Template,Goal),Node,Opts),
        apply_xmlpred(web_fetch_cancergrid,Node,Items).


% parse results
xmlpred('env:Envelope',_,[],translate(['env:Body','PrologResponse','List'])).
xmlpred('List',_,L,
        [mvlet(L1='*'),
         prolog(maplist(pnode_to_term,L1,L))]).

pnode_to_term(element('Atom',_,[X]),X).
pnode_to_term(element('Functor',[name=F],L),Term):-
        maplist(pnode_to_term,L,Args),
        Term=..[F|Args].

% -------------------- TESTS --------------------
% regression tests to ensure behaviour of module is correct;
% lines below here are not required for module functionality

disable_apitest(0).

unittest(test(cancergrid,
             [],
             (   ensure_loaded(bio(web_fetch_cancergrid)),
                 (   web_fetch_cancergrid:disable_apitest(1)
                 ->  true
                 ;   web_search_cancergrid(C,
                                           (   concept(C),wildcard_match('*tumour*',C)),
                                           Results,
                                           []),
                     forall(member(Result,Results),
                            format(' Result: ~w~n~n',[Result])))),
            true)).
unittest(test(cancergrid2,
             [],
             (   ensure_loaded(bio(web_fetch_cancergrid)),
                 (   web_fetch_cancergrid:disable_apitest(1)
                 ->  true
                 ;   web_search_cancergrid(C-P,
                                           (   concept(C),wildcard_match('*tumour*',C),subclass(C,P)),
                                           Results,
                                           []),
                     forall(member(Result,Results),
                            format(' Result: ~w~n~n',[Result])))),
            true)).
