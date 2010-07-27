/* -*- Mode: Prolog -*- */




:- module(web_fetch_wikipedia,[
                            web_search_wikipedia/3
                            ]).

:- use_module(bio(web_fetch)).
:- use_module(bio(xml_transform)).

url('http://api.search.wikipedia.com/WebSearchService/V1/webSearch').

%% web_fetch(wikipedia,+S,?Items)
%
%  queries Wikipedia based on search atom
%
%  each Item is a term of the form item(URL,Tags)
%
%  each Tag is of the format Name=Value
%
%  currently the only tag returned is 'snippet'
%  
%  
web_fetch:search_term_to_url(wikipedia,S,URLFull,_):-
        nonvar(S),
        member(query=Q,S),
        atom_concat('http://en.wikipedia.org/wiki/',Q,URLFull).

web_search_wikipedia(S,[Body],_Opts):-
        (   concat_atom(['wp-',S,'.html'],FN),
            expand_file_search_path(data_cache(FN),Out)
        ->  true
        ;   tmp_file(wiki,Out)),
        (   exists_file(Out)
        ->  true
        ;   sformat(Cmd,'scrape-wikipedia.pl "~w" > ~w',[S,Out]),
            debug(wikipedia,'cmd: ~w ',[Cmd]),
            shell(Cmd)),         % TODO: timeout
        open(Out,read,IO,[encoding(utf8)]),
        read_stream_to_codes(IO,Codes),
        atom_codes(Body,Codes),
        close(IO).

old_web_search_wikipedia(S,[Body],Opts):-
        web_fetch(wikipedia,[query=S|Opts],ResultNode,[maxResults(5)]),
        format(user_error,'   Fetch ~w~n',[S]),
        apply_xmlpred(web_fetch_wikipedia,ResultNode,page(S),Items),
        format(user_error,'   Fetched ~w~n',[S]),
        findall(X,member(body(X),Items),Body).


% xml transforms
%xmlpred('html',_,[Body],[let(Body=html)]).
xmlpred('html',_,[],translate(body)).
xmlpred('body',_,[body(Body)],[let(Body=xml_to_atom('.'))]).

% -------------------- TESTS --------------------
% regression tests to ensure behaviour of module is correct;
% lines below here are not required for module functionality

disable_apitest(0).

unittest(test(wikipedia,
             [],
             (   ensure_loaded(bio(web_fetch_wikipedia)),
                 writeln('Exact match:'),
                 web_search_wikipedia('Serosa',Results,[]),
                 forall(member(Result,Results),
                        format(' ~w~n~n',[Result])),
                 writeln('Bad search term:'),
                 web_search_wikipedia('zyxxxxHippocampus',Results2,[]),
                 forall(member(Result,Results2),
                        format(' ~w~n~n',[Result])),
                 writeln('redirect:'),
                 web_search_wikipedia('hippocampus',Results3,[]),
                 forall(member(Result,Results3),
                        format(' ~w~n~n',[Result]))),
            true)).

/** <module>
  @author Chris Mungall
  @version  $Revision: 1.4 $
  @date  $Date: 2005/10/04 00:08:31 $
  @license LGPL

  ---+ Name
  ---++ web_fetch_wikipedia
- wrapper for Google SOAP API

  ---+ Synopsis

  ==
  % uncomment the following line and put your own key here:
  % user:google_apikey('sdkhjaskldj//aljdalsdsjsdk').

  :- use_module(bio(web_fetch_wikipedia)).

  demo:-
      web_search_wikipedia('cancer differentiation',Results,[site='wikipedia.org']),
      forall(member(Item,Items),
             show_result_item(Item)).

  show_result_item(item(URL,Tags)):-
             format('URL: ~w~n',[URL]),
             member(snippet=Snippet,Tags),
             format('  snippet: ~w~n~n',[Snippet]).
  ==

  ---+ Description

  This passes a query to the Wikipedia REST API and parses the results


  ---+ SEE ALSO

  google module

  inspired by:
  http://www.hackdiary.com/archives/000070.html

  
    http://api.search.wikipedia.com/WebSearchService/V1/webSearch?appid=WikipediaDemo&query=%22margaret%20thatcher%22&site=wikipedia.org
  
**/
