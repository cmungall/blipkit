/* -*- Mode: Prolog -*- */


:- module(blipkit_web,[]).

:- use_module(bio(blipkit)).
:- use_module(bio(web_fetch)).
:- use_module(bio(web_fetch_google)).
:- use_module(bio(web_fetch_yahoo)).
:- use_module(bio(web_fetch_ncbi)).
:- use_module(bio(web_search_expander)).
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
