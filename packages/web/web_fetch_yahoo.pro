/* -*- Mode: Prolog -*- */

:- module(web_fetch_yahoo,[
                            web_search_yahoo/3
                            ]).

:- use_module(bio(web_fetch)).
:- use_module(bio(xml_transform)).

url('http://api.search.yahoo.com/WebSearchService/V1/webSearch').

%% web_fetch(yahoo,+S,?Items)
%
%  queries Yahoo based on search atom
%
%  each Item is a term of the form item(URL,Tags)
%
%  each Tag is of the format Name=Value
%
%  currently the only tag returned is 'snippet'
%  
%  
web_fetch:search_term_to_url(yahoo,S,URLFull,_):-
        nonvar(S),
        url(URL),
        Pairs=[appid='YahooDemo'|S],
        maplist(http_get_parameter,Pairs,Params),
        concat_atom(Params,'&',URLSuffix),
        concat_atom([URL,URLSuffix],'?',URLFull).

web_search_yahoo(S,Items,Opts):-
        web_fetch(yahoo,[query=S|Opts],ResultNode,[maxResults(5)]),
        apply_xmlpred(web_fetch_yahoo,ResultNode,Items).

% xml transforms
xmlpred('ResultSet',_,[],translate('Result')).
xmlpred('Result',_,[item(URL,[snippet=Snippet,title=Title,mime_type=MimeType])],
        [let(URL='Url'),
         let(Snippet='Summary'),
         let(Title='Title'),
         let(MimeType='MimeType')]).

% -------------------- TESTS --------------------
% regression tests to ensure behaviour of module is correct;
% lines below here are not required for module functionality

disable_apitest(0).

unittest(test(yahoo,
             [],
             (   ensure_loaded(bio(web_fetch_yahoo)),
                 web_search_yahoo('cancer differentiation',Results,[site='wikipedia.org']),
                 forall(member(Result,Results),
                        format(' ~w~n~n',[Result]))),
            true)).

/** <module>
  @author Chris Mungall
  @version  $Revision: 1.4 $
  @date  $Date: 2005/10/04 00:08:31 $
  @license LGPL

  ---+ Name
  ---++ web_fetch_yahoo
- wrapper for Google SOAP API

  ---+ Synopsis

  ==
  % uncomment the following line and put your own key here:
  % user:google_apikey('sdkhjaskldj//aljdalsdsjsdk').

  :- use_module(bio(web_fetch_yahoo)).

  demo:-
      web_search_yahoo('cancer differentiation',Results,[site='wikipedia.org']),
      forall(member(Item,Items),
             show_result_item(Item)).

  show_result_item(item(URL,Tags)):-
             format('URL: ~w~n',[URL]),
             member(snippet=Snippet,Tags),
             format('  snippet: ~w~n~n',[Snippet]).
  ==

  ---+ Description

  This passes a query to the Yahoo REST API and parses the results


  ---+ SEE ALSO

  google module

  inspired by:
  http://www.hackdiary.com/archives/000070.html

  
    http://api.search.yahoo.com/WebSearchService/V1/webSearch?appid=YahooDemo&query=%22margaret%20thatcher%22&site=wikipedia.org
  
**/