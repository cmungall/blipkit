/* -*- Mode: Prolog -*- */

:- module(web_fetch_cabig,
          [
           web_search_cabig/3,
           web_search_cabig/4
           ]).

:- use_module(bio(io)).
:- use_module(bio(web_fetch)).
:- use_module(bio(xml_transform)).
:- use_module(library('http/http_client')).

url(cabig,'http://cabio.nci.nih.gov/cacore32/GetXML').

%% web_fetch(cabig,+S,?Items)
%
%  ==
%  web_fetch(cabig,[select='Gene',where=where('Gene',id=1)],Results)
%  
%  ==
%
%  Search params:
%

web_fetch:search_term_to_url(cabig,S1,URLFull,_):-
        select(select=Select,S1,S0),
        select(where=Where,S0,_S),
        url(cabig,URL),
        convert_search_term(Where,WhereParam),
        sformat(URLFull,'~w?query=~w&~w',[URL,Select,WhereParam]).
web_fetch:search_term_to_url(cabig_direct,U0,U,_):-
        % THIS IS MADNESS - cabig have bad http URIs
        concat_atom([P1,P2],'&',U0),
        www_form_encode(P2,P2e),
        concat_atom([P1,P2e],'&',U).

convert_search_term(where(Class,Col=X),SOut1):-
        %www_form_encode(X,X1),
        X1=X,
        sformat(SOut,'~w[@~w=~w]',[Class,Col,X1]),
        www_form_encode(SOut,SOut1),
        !.
convert_search_term(S,S).


%% web_search_cabig(+DB,+S,?IDs)
%
%  ==
%  web_search_cabig('Gene',where('Gene',symbol='brca*'),Results)
%  ==
web_search_cabig(DB,S,Items):-
        web_search_cabig(DB,S,Items,[]).
web_search_cabig(DB,S,AllItems,Opts):-
        web_fetch(cabig,[select=DB,where=S|Opts],ResultNode),
        apply_xmlpred(web_fetch_cabig,ResultNode,Items),
        web_search_cabig_recursive(Items,AllItems,2).
web_search_cabig_recursive(_,[],DepthRemaining):-
        DepthRemaining =< 0,
        !.
web_search_cabig_recursive(Items,AllItemsBelow,DepthRemaining):-
        debug(web_fetch,'recursive[~w] on ~w',[DepthRemaining,Items]),
        DepthRemaining0 is DepthRemaining-1,
        findall(ItemsBelow,
                (   member(inst_sv(_,_,_,URL),Items),
                    URL \= '',  % hack
                    web_fetch(cabig_direct,URL,ResultNode),
                    apply_xmlpred(web_fetch_cabig,ResultNode,NextItems),
                    web_search_cabig_recursive(NextItems,ItemsBelow,DepthRemaining0)),
                ItemsBelowLOL),
        flatten([Items,ItemsBelowLOL],AllItemsBelow).

               
              


% eSearchResult/IdList/Id
xmlpred('xlink:httpQuery',_,[],translate(queryResponse)).   % TODO - proper ns handling
xmlpred(queryResponse,_,[],translate(class)).
xmlpred(class,_,[inst_of(ID,Class)],
        [let(Class=att(name)),
         translate(field,inst(ID,Class))]).
xmlpred(field,inst(ID,_),inst_sv(ID,N,V,Link),
        [let(N=att(name)),
         let(Link=att('xlink:href')),
         let(V='.'),
         prolog( (   N=id
                 ->  ID=V
                 ;   true))]).

% -------------------- TESTS --------------------
% regression tests to ensure behaviour of module is correct;
% lines below here are not required for module functionality

unittest(test(cabig_brca,
             [],
             (   ensure_loaded(bio(web_fetch_cabig)),
                 web_search_cabig('Gene',where('Gene',symbol='brca*'),Results),
                 forall(member(Result,Results),
                        format(' ID: "~w"~n',[Result]))),
            true)).

/** <module>
  @author Chris Mungall
  @version  $Revision: 1.4 $
  @date  $Date: 2005/10/04 00:08:31 $
  @license LGPL

  ---+ Name
  ---++ web_fetch_cabig
- wrapper for CABIG

  ---+ Synopsis

  ==
  :- use_module(bio(web_fetch_cabig)).

  ==

  ---+ Description

  See



  http://cabio.nci.nih.gov/cacore32/GetXML?query=Gene&Gene[@symbol=brca*]'
  GetXML?query=Tissue&Tissue[@organ=eye][@histology=neoplasia]
  
  
**/
