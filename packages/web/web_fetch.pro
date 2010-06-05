/* -*- Mode: Prolog -*- */




:- module(web_fetch,[
                     web_fetch/3,
                     web_fetch/4,
                     http_get_parameter/2
                    ]).

:- multifile web_fetch/4.
:- multifile search_term_to_url/4.

:- use_module(library('http/http_client')).
:- use_module(library(sgml_write)).
:- use_module(library(url)).

%% web_fetch(+Resource,+SearchTerm,?Items)
web_fetch(M,S,Items):-
        web_fetch(M,S,Items,[]).

%% web_fetch(+Resource,+SearchTerms,?Items,+Opts)
%
%  ==
%  web_fetch(ncbi,[service=efetch,db=omim,id=601100,retmode=xml,rettype=full],Results)
%  ==
%  
%  
web_fetch(Resource,S,ResultNode,Opts):-
        nonvar(S),
        search_term_to_url(Resource,S,URL,[]),
        debug(web_fetch,'URL: ~w',[URL]),
        http_get(URL,R,[]),
        debug(web_fetch,'Fetched: ~w',[URL]),
        parse_file_contents(R,ResultNode,Opts).

parse_file_contents(R,ResultNode,Opts):-
        member(noparse(1),Opts),
        !,
        ResultNode=R.
parse_file_contents(R,ResultNode,Opts):-
        pre_filter_xml(R,R2,Opts),
        tmp_file(weboutxml,OutFile),
        debug(web_fetch,'opening: ~w',[OutFile]),
        open(OutFile,write,OutStream2,[]),
        debug(web_fetch,'writing: ~w',[R2]),
        write(OutStream2,R2),
        nl(OutStream2),
        close(OutStream2),
        debug(web_fetch,'closing',[]),
        load_structure(OutFile,ResultNode,[dialect(xml),space(remove)]).

%        load_structure(OutFile,ResultNode,[dialect(xmlns),space(remove)]). -- cheat for now TODO: fix

http_get_parameter(X=Y,S):-
        www_form_encode(Y,YEnc),
        concat_atom([X,YEnc],'=',S).

pre_filter_xml(R,R,Opts):-
        \+ member(prefilter(1),Opts),
        !.
pre_filter_xml(R,R2,_):-
        debug(web_fetch,'prefiltering:',[]),
        sub_atom(R,Bef,_Len,_After,'<?xml'),
        sub_atom(R,Bef,_,0,R2),
        debug(web_fetch,'prefiltered: ~w',[R2]).


/** <module>
  @author Chris Mungall
  @version  $Revision: 1.4 $
  @date  $Date: 2005/10/04 00:08:31 $
  @license LGPL

  ---+ Name
  ---++ web_fetch
- generic web fething wrapper

  ---+ Synopsis

  ==
  :- use_module(bio(web_fetch)).
  ==

  ---+ Description

  constructs URLs, fetches results

  don't use this directly, use one of the following:
  
  ---+ See also

  - web_fetch_google
  - web_fetch_yahoo
  - web_fetch_ncbi

  
**/
