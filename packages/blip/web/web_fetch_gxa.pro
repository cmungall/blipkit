/* -*- Mode: Prolog -*- */




:- module(web_fetch_gxa,
          [
           web_search_gxa/3,
           web_search_gxa/4,
           web_fetch_gxa_db_by_ids/2,
           web_fetch_gxa_omim_by_ids/1,
           web_fetch_gxa_pubmed_by_ids/1,
           gilist_to_accmap/2
           ]).

:- use_module(bio(io)).
:- use_module(bio(web_fetch)).
:- use_module(bio(xml_transform)).

url(gxa,'http://www.ebi.ac.uk/gxa/api').
url_param(db).
url_param(cmd).
url_param(term).
url_param(retmax).

%% web_fetch(gxa,+S,?Items)
%
%  ==
%  web_fetch(gxa,[service=efetch,db=omim,id=601100,retmode=xml,rettype=full],
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
web_fetch:search_term_to_url(gxa,S1,URLFull,_):-
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

web_fetch_gxa_db_by_ids(DB,IDs):-
        flatten_ids(IDs,ID),
        % prefiltering is necessary for homolgene due to gxa bug
        (   DB=homologene
        ->  Opts=[prefilter(1)]
        ;   Opts=[]),
        web_fetch(gxa,[service=efetch,db=DB,id=ID,retmode=xml,rettype=full],Items,Opts),
        db_format(DB,Format),
        format_module_xmlmap(Format,Schema,XmlMap),
        ensure_loaded(bio(XmlMap)),
        apply_xmlpred(XmlMap,Items,Terms),
        Schema:maplist(assert,Terms).


%% web_search_gxa(+DB,+S,?IDs)
%
%  ==
%  web_search_gxa(homologene,query('BRCA1','gene name'),Results)
%  ==
web_search_gxa(DB,S,Items):-
        web_search_gxa(DB,S,Items,[]).
web_search_gxa(DB,S,Items,Opts):-
        convert_search_term(S,SExpanded),
        web_fetch(gxa,[service=esearch,db=DB,term=SExpanded|Opts],ResultNode),
        apply_xmlpred(web_fetch_gxa,ResultNode,Items).
web_fetch_gxa_omim_by_ids(IDs):- web_fetch_gxa_db_by_ids(omim,IDs).
web_fetch_gxa_pubmed_by_ids(IDs):- web_fetch_gxa_db_by_ids(pubmed,IDs).

convert_search_term(query(S,Field),SOut):-
        sformat(SOut,'~w[~w]',[S,Field]),
        !.
convert_search_term(S,S).

% GXA sometimes returns things embedded in html, aarggh
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
        web_fetch(gxa,[service=efetch,db=nucleotide,rettype=acc,id=IDAtom],ResultAtom,[noparse(1)]),
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
            (   ensure_loaded(bio(web_fetch_gxa)),
                gilist_to_accmap([2,3,4],Accs),
                writeln(Accs)),
            true)).

unittest(test(omim_cancer,
             [],
             (   ensure_loaded(bio(web_fetch_gxa)),
                 web_search_gxa(omim,cancer,Results),
                 forall(member(Result,Results),
                        format(' ID: "~w"~n',[Result]))),
            true)).
unittest(test(homologene_search,
             [],
             (   ensure_loaded(bio(web_fetch_gxa)),
                 web_search_gxa(homologene,query('BRCA1','gene name'),Results),
                 forall(member(Result,Results),
                        format(' ID: "~w"~n',[Result]))),
            true)).

unittest(test(homologene_BRCA1,
             [],
             (   ensure_loaded(bio(web_fetch_gxa)),
                 web_fetch_gxa_db_by_ids(homologene,'5276'),
                 write_biofile(homol_db:pro,_)),
            true)).
/** <module> wrapper for GXA REST interface

  ---+ Synopsis

==
  :- use_module(bio(web_fetch_gxa)).
==

  ---+ Description

==
http://www.ebi.ac.uk/gxa/api?geneIs=p53&downInOrgansimpart=kidney&upInSex=male  
==

returns

  ==

  {"results": [{"gene":{"name":"Trp53bp1",
                        "id":"ENSMUSG00000043909",
                        "orthologs": ["AGAP001466","ENSBTAG00000021304","ENSDARG00000079000","ENSGALG00000008521","ENSRNOG00000013837","ENSG00000067369"],
                        "ensemblGeneId":"ENSMUSG00000043909","goTerms": ["DNA repair","ETC"],
                        "interProIds": ["IPR001357","IPR002114","IPR014722","IPR015125"],
                        "interProTerms": ["Translation protein SH3-like, subgroup","Tumour suppressor p53-binding protein-1 Tudor","BRCT","Phosphotransferase system, HPr serine phosphorylation site"],
                        "keywords": ["3D-structure","Activator","Alternative splicing","DNA repair","Kinetochore","Nucleus","Repeat","Transcription regulation","Transcription","Phosphoprotein","Methylation","DNA-binding","DNA damage"],
                         "uniprotIds": ["P70399","Q3UG49","Q3V1P7","Q8BZ87","Q8C0U1","Q8BLA0"],
                         "synonyms": ["Tp53bp1","Trp53bp1"],
                         "goIds": ["GO:0000776","GO:0000777","GO:0003684","GO:0005657","GO:0006281","GO:0006355","GO:0042162","GO:0008134","GO:0006350","GO:0005737","GO:0005634"],
                         "dbxrefs": ["MGI:1351320"],"emblIds": ["AJ414734","AK029832","AK036324","AK132323","AL929059","BC079906","U67885","BC035206","AK148123","AK045851"],
  
                         "expressions": [{"ef":"sex","efv":"male",
                                          "experiments": [{"expression":"UP","pvalue":0.0472443435855129,"accession":"E-GEOD-3530"},
                                                          {"expression":"UP","pvalue":8.81429597952978E-7,"accession":"E-GEOD-3463"}],
                                                           "upExperiments":2,"downExperiments":0,"upPvalue":8.814295711090381E-7,"downPvalue":0.0}]},


  "expressions": [{"ef":"sex","efv":"male","experiments": [{"expression":"DOWN","pvalue":9.16013020249447E-5,"accession":"E-GEOD-2466"},{"expression":"UP","pvalue":0.0106074023749639,"accession":"E-GEOD-1295"}],"upExperiments":1,"downExperiments":1,"upPvalue":0.01060740277171135,"downPvalue":9.160130139207467E-5}]}],"totalResultGenes":18,"numberOfResultGenes":18,"startingFrom":0}

  ==
  
**/
