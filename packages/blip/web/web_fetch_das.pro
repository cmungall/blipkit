/* -*- Mode: Prolog -*- */

:- module(web_fetch_das,
          [
           web_search_das/3,
           web_search_das/4,
           web_fetch_das_db_by_ids/2,
           web_fetch_das_omim_by_ids/1,
           web_fetch_das_pubmed_by_ids/1
           ]).

:- use_module(bio(io)).
:- use_module(bio(genomic)).
:- use_module(bio(web_fetch)).
:- use_module(bio(xml_transform)).

server_url(reference,'http://das.biopackages.net/').


%% web_fetch(das,+S,?Items)
%
%  ==
%  web_fetch(das,[service=efetch,db=omim,id=601100,retmode=xml,rettype=full],
%  
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
web_fetch:search_term_to_url(das,S1,URLFull,_):-
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

web_fetch_das_db_by_ids(DB,IDs):-
        flatten_ids(IDs,ID),
        % prefiltering is necessary for homolgene due to das bug
        (   DB=homologene
        ->  Opts=[prefilter(1)]
        ;   Opts=[]),
        web_fetch(das,[service=efetch,db=DB,id=ID,retmode=xml,rettype=full],Items,Opts),
        db_format(DB,Format),
        format_module_xmlmap(Format,Schema,XmlMap),
        ensure_loaded(bio(XmlMap)),
        apply_xmlpred(XmlMap,Items,Terms),
        Schema:maplist(assert,Terms).


%% web_search_das(+DB,+S,?IDs)
%
%  ==
%  web_search_das(homologene,query('BRCA1','gene name'),Results)
%  ==
web_search_das(DB,S,Items):-
        web_search_das(DB,S,Items,[]).
web_search_das(DB,S,Items,Opts):-
        convert_search_term(S,SExpanded),
        web_fetch(das,[service=esearch,db=DB,term=SExpanded|Opts],ResultNode),
        apply_xmlpred(web_fetch_das,ResultNode,Items).
web_fetch_das_omim_by_ids(IDs):- web_fetch_das_db_by_ids(omim,IDs).
web_fetch_das_pubmed_by_ids(IDs):- web_fetch_das_db_by_ids(pubmed,IDs).

convert_search_term(query(S,Field),SOut):-
        sformat(SOut,'~w[~w]',[S,Field]),
        !.
convert_search_term(S,S).

% eSearchResult/IdList/Id
xmlpred('FEATURES',_,[],translate('FEATURE')).
xmlpred('FEATURE',_,feature(ID,ID,Type),
        [let(ID=att(uri)),
         let(TypeX=att(type)),
         prolog((concat_atom(L,TypeX),
                 reverse(L,[Type|_]))),
         translate('LOC',in(ID)),
         translate('PARENT',in(ID)),
         translate('PROP',in(ID))
        ]).
xmlpred('LOC',in(F),featureloc(F,Seg,S0,E0,Str0,0,0,[]),
        [let(Seg=att(segment)),
         let(Range=att(range)),
         prolog((concat_atom([S,E,Str],':',Range),
                 atom_number(S,SNum),
                 atom_number(E,ENum),
                 atom_number(Range,RangeNum),
                 map_coordsystem(gff(SNum,ENum,Str),interbase(S0,E0,Str0)))))]).
xmlpred('PROP',in(F),featureprop(F,Key,Val),
        [let(Key=att(key)),
         let(Val=att(value))]).
xmlpred('PARENT',in(F),feature_relationship(F,Parent,part_of,0),
        let(Parent=att(uri))).

% -------------------- TESTS --------------------
% regression tests to ensure behaviour of module is correct;
% lines below here are not required for module functionality

unittest(test(omim_cancer,
             [],
             (   ensure_loaded(bio(web_fetch_das)),
                 web_search_das(omim,cancer,Results),
                 forall(member(Result,Results),
                        format(' ID: "~w"~n',[Result]))),
            true)).
unittest(test(homologene_search,
             [],
             (   ensure_loaded(bio(web_fetch_das)),
                 web_search_das(homologene,query('BRCA1','gene name'),Results),
                 forall(member(Result,Results),
                        format(' ID: "~w"~n',[Result]))),
            true)).

unittest(test(homologene_BRCA1,
             [],
             (   ensure_loaded(bio(web_fetch_das)),
                 web_fetch_das_db_by_ids(homologene,'5276'),
                 write_biofile(homol_db:pro,_)),
            true)).
/** <module>
  @author Chris Mungall
  @version  $Revision: 1.4 $
  @date  $Date: 2005/10/04 00:08:31 $
  @license LGPL

  ---+ Name
  ---++ web_fetch_das
- wrapper for DAS

  ---+ Synopsis

  ==
  :- use_module(bio(web_fetch_das)).

  ==

  ---+ Description

  http://das.biopackages.net/das/genome/human/17/feature?name=ACTA1

  See

  http://www.biodas.org/wiki/DAS/2
  
**/
