/* -*- Mode: Prolog -*- */




:- module(web_fetch_ncbi,
          [
           web_search_ncbi/3,
           web_search_ncbi/4,
           web_fetch_ncbi_db_by_ids/2,
           web_fetch_ncbi_omim_by_ids/1,
           web_fetch_ncbi_pubmed_by_ids/1,
           gilist_to_accmap/2
           ]).

:- use_module(bio(io)).
:- use_module(bio(web_fetch)).
:- use_module(bio(xml_transform)).

url(esearch,'http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi').
url(efetch,'http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi').
url_param(db).
url_param(cmd).
url_param(term).
url_param(retmax).

%% web_fetch(ncbi,+S,?Items)
%
%  ==
%  web_fetch(ncbi,[service=efetch,db=omim,id=601100,retmode=xml,rettype=full],
%  ==
%
%  Search params:
%
%  service - efetch/esearch
%  db
%  retmode
%  rettype
%  
%  
%  

% (+,+,?,+)
web_fetch:search_term_to_url(ncbi,S1,URLFull,_):-
        select(service=Service,S1,S),
        url(Service,URL),
        maplist(http_get_parameter,S,Params),
        concat_atom(Params,'&',URLSuffix),
        concat_atom([URL,URLSuffix],'?',URLFull).

flatten_ids(IDs,ID):-
        is_list(IDs),
        !,
        concat_atom(IDs,',',ID).
flatten_ids(ID,ID).

db_format(gene,entrezgene).
db_format(X,X):- !.

web_fetch_ncbi_db_by_ids(DB,IDs):-
        flatten_ids(IDs,ID),
        % prefiltering is necessary for homolgene due to ncbi bug
        (   DB=homologene
        ->  Opts=[prefilter(1)]
        ;   Opts=[]),
        web_fetch(ncbi,[service=efetch,db=DB,id=ID,retmode=xml,rettype=full],Items,Opts),
        db_format(DB,Format),
        format_module_xmlmap(Format,Schema,XmlMap),
        ensure_loaded(bio(XmlMap)),
        apply_xmlpred(XmlMap,Items,Terms),
        Schema:maplist(assert,Terms).


%% web_search_ncbi(+DB,+S,?IDs)
%
%  ==
%  web_search_ncbi(homologene,query('BRCA1','gene name'),Results)
%  ==
web_search_ncbi(DB,S,Items):-
        web_search_ncbi(DB,S,Items,[]).
web_search_ncbi(DB,S,Items,Opts):-
        convert_search_term(S,SExpanded),
        web_fetch(ncbi,[service=esearch,db=DB,term=SExpanded|Opts],ResultNode),
        apply_xmlpred(web_fetch_ncbi,ResultNode,Items).
web_fetch_ncbi_omim_by_ids(IDs):- web_fetch_ncbi_db_by_ids(omim,IDs).
web_fetch_ncbi_pubmed_by_ids(IDs):- web_fetch_ncbi_db_by_ids(pubmed,IDs).

convert_search_term(query(S,Field),SOut):-
        sformat(SOut,'~w[~w]',[S,Field]),
        !.
convert_search_term(S,S).

% NCBI sometimes returns things embedded in html, aarggh
xmlpred(html,_,[],translate(head)).
xmlpred(head,_,[],translate(sSearchResult)).
% eSearchResult/IdList/Id
xmlpred(eSearchResult,_,[],
        [let(_C='Count'),
         let(_RM='RetMax'),
         let(_RS='RetStart'),
        translate('IdList')]).
xmlpred('IdList',_,[],
        translate('Id')).
xmlpred('Id',_,ID,
        [let(ID='.')]).

%% gilist_to_accmap(+GIs,?Map) is det
% given a list of GI numbers, return a map from GI numbers to accessions
% @param Map [GI1-Acc1,GI2-Acc2,....]
gilist_to_accmap(GIs,Map):-
        concat_atom(GIs,',',IDAtom),
        web_fetch(ncbi,[service=efetch,db=nucleotide,rettype=acc,id=IDAtom],ResultAtom,[noparse(1)]),
        concat_atom(Accs,'\n',ResultAtom),
        zip(GIs,Accs,Map).

zip([],_,[]):- !.
%zip([],L,_):- throw(lists_must_be_same_size(L)).
zip([A|L],[B|M],[A-B|N]):-
        !,
        zip(L,M,N).






% -------------------- TESTS --------------------
% regression tests to ensure behaviour of module is correct;
% lines below here are not required for module functionality

unittest(test(gi,
            [],
            (   ensure_loaded(bio(web_fetch_ncbi)),
                gilist_to_accmap([2,3,4],Accs),
                writeln(Accs)),
            true)).

unittest(test(omim_cancer,
             [],
             (   ensure_loaded(bio(web_fetch_ncbi)),
                 web_search_ncbi(omim,cancer,Results),
                 forall(member(Result,Results),
                        format(' ID: "~w"~n',[Result]))),
            true)).
unittest(test(homologene_search,
             [],
             (   ensure_loaded(bio(web_fetch_ncbi)),
                 web_search_ncbi(homologene,query('BRCA1','gene name'),Results),
                 forall(member(Result,Results),
                        format(' ID: "~w"~n',[Result]))),
            true)).

unittest(test(homologene_BRCA1,
             [],
             (   ensure_loaded(bio(web_fetch_ncbi)),
                 web_fetch_ncbi_db_by_ids(homologene,'5276'),
                 write_biofile(homol_db:pro,_)),
            true)).
/** <module> wrapper for NCBI eutils

  ---+ Synopsis

==
  :- use_module(bio(web_fetch_ncbi)).
==

  ---+ Description

  ---++ EUtils URLs

  http://www.ncbi.nlm.nih.gov/entrez/query/static/linking.html

  http://eutils.ncbi.nlm.nih.gov/entrez/query/static/esearch_help.html
  
  examples

  http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term=cancer&reldate=60&datetype=edat&retmax=100&usehistory=y

  http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&id=11748933,11700088&retmode=xml  

  by GeneID:
  http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=homologene&term=373983%5Bgene%20id%5D&retmode=xml
  http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=homologene&term=BRCA1%5Bgene%20name%5D&retmode=xml
  http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=homologene&id=5276

  GEO:
  
  http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=gds&term=GSE[ETYP]&retmax=5000&usehistory=y
  
  ---++ TODO

  sgml2pl complains about lack of DTD

  ---++ TODO
  
  As part of an ongoing effort to ensure efficient access to the Entrez Utilities (E-utilities) by all users, NCBI has decided to change the usage policy for the E-utilities effective June 1, 2010. Effective on June 1, 2010, all E-utility requests, either using standard URLs or SOAP, must contain non-null values for both the &tool and &email parameters. Any E-utility request made after June 1, 2010 that does not contain values for both parameters will return an error explaining that these parameters must be included in E-utility requests.

The value of the &tool parameter should be a URI-safe string that is the name of the software package, script or web page producing the E-utility request.

The value of the &email parameter should be a valid e-mail address for the appropriate contact person or group responsible for maintaining the tool producing the E-utility request.

NCBI uses these parameters to contact users whose use of the E-utilities violates the standard usage policies described athttp://eutils.ncbi.nlm.nih.gov/entrez/query/static/eutils_help.html#UserSystemRequirements. These usage policies are designed to prevent excessive requests from a small group of users from reducing or eliminating the wider community's access to the E-utilities. NCBI will attempt to contact a user at the e-mail address provided in the &email parameter prior to blocking access to the E-utilities.

NCBI realizes that this policy change will require many of our users to change their code. Based on past experience, we anticipate that most of our users should be able to make the necessary changes before the June 1, 2010 deadline. If you have any concerns about making these changes by that date, or if you have any questions about these policies, please contact eutilities@ncbi.nlm.nih.gov.

  
**/
