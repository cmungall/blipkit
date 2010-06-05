/* -*- Mode: Prolog -*- */


:- module(web_fetch_google,[
                  web_search_google/2,
                  web_search_google/3
                 ]).

:- use_module(bio(web_fetch)).
:- use_module(bio(xml_transform)).
:- use_module(library('http/http_client')).
:- use_module(library(sgml_write)).

url('http://api.google.com/search/beta2').

%% web_search_google(+S,?Items)
%
%  queries google based on search atom
%
%  each Item is a term of the form item(URL,Tags)
%
%  each Tag is of the format Name=Value
%
%  currently the only tag returned is 'snippet'
%  
%  
web_search_google(S,Items):-
        web_search_google(S,Items,[]).
%% web_search_google(+S,?Items,+Opts)
%
%  As web_search_google/2, with the following allowed options:
%
%  
%  * maxResults(+Num)
%  * safeSearch(+Bool)
%  * filter(+Bool)
%  @\list
web_search_google(S,Items,Opts):-
        nonvar(S),
        url(URL),
        user:google_apikey(Key),
        post_data(Key,S,Opts,Node),
        tmp_file(googleinxml,File),
        open(File,write,OutStream,[]),
        sgml_write(OutStream,Node,[]),
        close(OutStream),
        http_post(URL,file('text/xml',File),R,[]),
        tmp_file(googleoutxml,OutFile),
        open(OutFile,write,OutStream2,[]),
        write(OutStream2,R),
        nl(OutStream2),
        close(OutStream2),
        load_structure(OutFile,ResultNode,[dialect(xml),space(remove)]),
        apply_xmlpred(web_fetch_google,ResultNode,Items).

xmlpred('SOAP-ENV:Envelope',_,[],translate(['//',item])).
xmlpred(item,_,[item(URL,[snippet=Snippet])],
        [let(URL='URL'),
         let(Snippet=snippet)]).

post_data(ApiKey,Search,Opts,E):-
        lookup_opt(maxResults,Opts,MaxResults1,10),
        (   number(MaxResults1)
        ->  atom_number(MaxResults,MaxResults1)
        ;   MaxResults=MaxResults1),
        lookup_opt(safeSearch,Opts,SafeSearch,false),
        lookup_opt(filter,Opts,Filter,true),
        E=element('SOAP-ENV:Envelope',
                  ['xmlns:SOAP-ENV'='http://schemas.xmlsoap.org/soap/envelope/',
                   'xmlns:xsi'='http://www.w3.org/1999/XMLSchema-instance',
                   'xmlns:xsd'='http://www.w3.org/1999/XMLSchema'],
                  [element('SOAP-ENV:Body',
                           [],
                           [element('ns1:doGoogleSearch',
                                    ['xmlns:ns1'='urn:GoogleSearch',
                                     'SOAP-ENV:encodingStyle'='http://schemas.xmlsoap.org/soap/encoding/'],
                                    [element(key, ['xsi:type'='xsd:string'],
                                             [ApiKey]),
                                     element(q, ['xsi:type'='xsd:string'],
                                             [Search]),
                                     element(start, ['xsi:type'='xsd:int'],
                                             ['0']),
                                     element(maxResults, ['xsi:type'='xsd:int'],
                                             [MaxResults]),
                                     element(filter, ['xsi:type'='xsd:boolean'],
                                             [Filter]),
                                     element(restrict, ['xsi:type'='xsd:string'], []),
                                     element(safeSearch,
                                             ['xsi:type'='xsd:boolean'],
                                             [SafeSearch]),
                                     element(lr,
                                             ['xsi:type'='xsd:string'], []),
                                     element(ie,
                                             ['xsi:type'='xsd:string'],
                                             [latin1]),
                                     element(oe, ['xsi:type'='xsd:string'],
                                             [latin1])])])]).

lookup_opt(N,Opts,V,_):-
        T =.. [N,V],
        member(T,Opts),
        !.
lookup_opt(_,_,V,V). % use default
        

% -------------------- TESTS --------------------
% regression tests to ensure behaviour of module is correct;
% lines below here are not required for module functionality

disable_apitest(0).

xxunittest(test(parse,
             [],
             (
               ensure_loaded(bio(web_fetch_google)),
               ensure_loaded(bio(xml_transform)),
               File='google-results.xml',
               load_structure(File,Node,[dialect(xml),space(remove)]),
               apply_xmlpred(web_fetch_google,Node,Items),
               forall(member(item(URL,Text),Items),
                      format('~w  -- "~w"~n',[URL,Text]))),
            true)).

unittest(test(google,
             [],
             (   ensure_loaded(bio(web_fetch_google)),
                 (   web_fetch_google:disable_apitest(1)
                 ->  true
                 ;   web_search_google('blipkit',Results,[maxResults(5)]),
                     forall(member(Result,Results),
                            format(' ~w~n~n',[Result])))),
            true)).
/** <module>
  @author Chris Mungall
  @version  $Revision: 1.4 $
  @date  $Date: 2005/10/04 00:08:31 $
  @license LGPL

  ---+ Name
  ---++ web_fetch_google
- wrapper for Google SOAP API

  ---+ Synopsis

  ==
  % uncomment the following line and put your own key here:
  % user:google_apikey('sdkhjaskldj//aljdalsdsjsdk').

  :- use_module(bio(web_fetch_google)).

  demo:-
      web_search_google('google blip',Items),
      forall(member(Item,Items),
             show_result_item(Item)).

  show_result_item(item(URL,Tags)):-
             format('URL: ~w~n',[URL]),
             member(snippet=Snippet,Tags),
             format('  snippet: ~w~n~n',[Snippet]).
  ==

  ---+ Description

  This passes a query to the google SOAP API and parses the results

  ---++ Setup
  
  put google_apikey/1 in your .plrc or bioconf.pro

  ---+ NEWS

  Evil Google:
  
  http://radar.oreilly.com/archives/2006/12/google_depreciates_SOAP_API.html
  
  ---+ TODO

  ---++ Yahoo search

  Uses REST API; eg
  
  http://api.search.yahoo.com/WebSearchService/V1/webSearch?appid=YahooDemo&query=%22margaret%20thatcher%22&site=wikipedia.org
  
**/
