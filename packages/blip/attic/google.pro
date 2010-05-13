/* -*- Mode: Prolog -*- */




:- module(google,[
                  google_search/2,
                  google_search/3
                 ]).

:- use_module(library('http/http_client')).
:- use_module(library(sgml_write)).
:- use_module(bio(xml_transform)).

url('http://api.google.com/search/beta2').

%% google_search(+S,?Items)
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
google_search(S,Items):-
        google_search(S,Items,[]).
%% google_search(+S,?Items,+Opts)
%
%  As google_search/2, with the following allowed options:
%
%  
%  * maxResults(+Num)
%  * safeSearch(+Bool)
%  * filter(+Bool)
%  @\list
google_search(S,Items,Opts):-
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
        %OutFile='/users/cjm/tmp/foo',
        open(OutFile,write,OutStream2,[]),
        write(OutStream2,R),
        nl(OutStream2),
        close(OutStream2),
        writeln(parsing(OutFile)),
        load_structure(OutFile,ResultNode,[dialect(xml),space(remove)]),
        writeln(ResultNode),
        apply_xmlpred(google,ResultNode,Items).

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
               ensure_loaded(bio(google)),
               ensure_loaded(bio(xml_transform)),
               File='google-results.xml',
               load_structure(File,Node,[dialect(xml),space(remove)]),
               apply_xmlpred(google,Node,Items),
               forall(member(item(URL,Text),Items),
                      format('~w  -- "~w"~n',[URL,Text]))),
            true)).

unittest(test(google,
             [],
             (   ensure_loaded(bio(google)),
                 (   google:disable_apitest(1)
                 ->  true
                 ;   google_search('blipkit',Results,[maxResults(5)]),
                     forall(member(Result,Results),
                            format(' ~w~n~n',[Result])))),
            true)).
/** <module>
  @author Chris Mungall
  @version  $Revision: 1.4 $
  @date  $Date: 2005/10/04 00:08:31 $
  @license LGPL

  ---+ Name
  ---++ google
- wrapper for Google SOAP API

  ---+ Synopsis

  ==
  % uncomment the following line and put your own key here:
  % user:google_apikey('sdkhjaskldj//aljdalsdsjsdk').

  :- use_module(bio(google)).

  demo:-
      google_search('google blip',Items),
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

  ---+ TODO

  ---++ Yahoo search

  Uses REST API; eg
  
  http://api.search.yahoo.com/WebSearchService/V1/webSearch?appid=YahooDemo&query=%22margaret%20thatcher%22&site=wikipedia.org
  
**/