/* -*- Mode: Prolog -*- */


:- module(blipkit_web,[]).

:- use_module(bio(blipkit)).
:- use_module(bio(web_fetch)).
:- use_module(bio(web_fetch_google)).
:- use_module(bio(web_fetch_yahoo)).
:- use_module(bio(web_fetch_ncbi)).
:- use_module(bio(web_search_expander)).
:- use_module(bio(stats_distributions)).
:- use_module(library(sgml_write)).

:- blip('web-expand-query',
        'translate a term to a search engine term',
        [atom([service],Service)],
        [Term],
        (
         replace_search_term_atom(Term,Service,SearchTerm),
         writeln(SearchTerm))).


:- blip('web-fetch',
        'web search',
        [atom([service],Service)],
        SearchTerms,
        (
          maplist(split_on_eq,SearchTerms,SearchParams),
          web_fetch(Service,SearchParams,ResultNode),
          xml_write(user_output,ResultNode,[]))).

split_on_eq(S,A=B):- concat_atom([A,B],'=',S).

blipkit:example('blip web-fetch -service ncbi db=gene service=efetch id=7157 retmode=xml',
                'see http://www.ncbi.nlm.nih.gov/entrez/query/static/linking.html').

:- blip('search-ncbi',
        'query using ncbi web services',
        [atom([service],Service,pubmed)],
        SearchTerms,
        (
         atomic_list_concat(SearchTerms,' ',SearchAtom),
         web_search_ncbi(Service,SearchAtom,IDs),
         forall(member(ID,IDs),
                writeln(ID)))).


:- blip('pubmed-corr',
        'test correlation of terms',
        [term(popsize,PopSize,9999999)],
        [T1,T2],
         (
         test_corr(T1,T2,PopSize,P),
         writeln(inf=P))).

test_corr(T1,T2,PopSize,P) :-
        atomic_list_concat([T1,T2],' ',T3),
        pubmed_count(T1,T1c),
        pubmed_count(T2,T2c),
        pubmed_count(T3,T3c),
        debug(pubmed,'|T1|=~w |T2|=~w |Both|=~w',[T1c,T2c,T3c]),
        mutual_information(T3c,T1c,T2c,PopSize,P).
%p_value_by_hypergeometric(T3c,T1c,T2c,PopSize,P).

pubmed_count(T,C) :-
        web_search_ncbi(pubmed,T,L),
        member(count(C),L),
        !.

mutual_information_quick(K,N,M,_Pop,I) :-
        I is log(K/(M*N))/log(2).

mutual_information(K,N,M,Pop,I) :-
        I is log((K/Pop)/((M/Pop)*(N/Pop)))/log(2).


/*
gtest(K,N,M,Pop,P) :-
        log(K/ ((N*
        Nr is Pop-N,
        Mr is Pop-M,
        E11 is ((
*/
        

/** <module>   
  @author Chris Mungall
  @version  $Revision: 1.2 $
  @date  $Date: 2006/03/25 01:57:15 $
  @license LGPL

  ---+ Name
  ---++ blipkit
- simple interface to blip module functionality

  ---+ Description

  this is a submodule of blipkit for handling web*/
