/* -*- Mode: Prolog -*- */




:- module(web_search_yahoo,[
                            web_search/3,
                            web_search/4
                            ]).

:- use_module(library('http/http_client')).
:- use_module(library(sgml_write)).
:- use_module(library(url)).
:- use_module(bio(xml_transform)).

url('http://api.search.yahoo.com/WebSearchService/V1/webSearch').

%% web_search(yahoo,+S,?Items)
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
web_search(M,S,Items):-
        web_search(M,S,Items,[]).

web_search(yahoo,S,Items,_Opts):-
        nonvar(S),
        url(URL),
        Pairs=[appid='YahooDemo'|S],
        maplist(http_get_parameter,[appid='YahooDemo'|S],Params),
        concat_atom(Params,'&',URLSuffix),
        concat_atom([URL,URLSuffix],'?',URLFull),
        writeln(URLFull),
        http_get(URLFull,R,[]),
        tmp_file(googleoutxml,OutFile),
        open(OutFile,write,OutStream2,[]),
        write(OutStream2,R),
        nl(OutStream2),
        close(OutStream2),
        load_structure(OutFile,ResultNode,[dialect(xml),space(remove)]),
        %writeln(ResultNode),
        apply_xmlpred(web_search_yahoo,ResultNode,Items).
%Items=ResultNode.

xmlpred('ResultSet',_,[],translate('Result')).
xmlpred('Result',_,[item(URL,[snippet=Snippet,title=Title,mime_type=MimeType])],
        [let(URL='Url'),
         let(Snippet='Summary'),
         let(Title='Title'),
         let(MimeType='MimeType')]).

http_get_parameter(X=Y,S):-
        www_form_encode(Y,YEnc),
        concat_atom([X,YEnc],'=',S).

% -------------------- TESTS --------------------
% regression tests to ensure behaviour of module is correct;
% lines below here are not required for module functionality

disable_apitest(0).

unittest(test(yahoo,
             [],
             (   ensure_loaded(bio(web_search_yahoo)),
                 web_search(yahoo,[query='cancer differentiation',site='wikipedia.org'],Results,[maxResults(5)]),
                 forall(member(Result,Results),
                        format(' ~w~n~n',[Result]))),
            true)).
/** <module>
  @author Chris Mungall
  @version  $Revision: 1.4 $
  @date  $Date: 2005/10/04 00:08:31 $
  @license LGPL

  ---+ Name
  ---++ web_search_yahoo
- wrapper for Google SOAP API

  ---+ Synopsis

  ==
  % uncomment the following line and put your own key here:
  % user:google_apikey('sdkhjaskldj//aljdalsdsjsdk').

  :- use_module(bio(web_search_yahoo)).

  demo:-
      web_search(yahoo,[query='margaret thatcher',site='wikipedia.org'],Items),
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