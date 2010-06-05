:- module(web_search_expander,[
                               replace_search_term_atom/2,
                               replace_search_term_atom/3,
                               create_search_term_atom/3,
                               create_search_term_atom/2
                               ]).
:- use_module(bio(ontol_db)).
:- use_module(bio(bioprolog_util)).
:- use_module(bio(metadata_db)).

/**

  http://www.searchengineshowdown.com/features/byfeature.shtml

  not much use if engines don't allow long queries; google has 32 word limit. 

  hotbot: max string?
  "apoptotic programmed cell death" OR "signaling (initiator) caspase activity" OR "type I programmed cell death" OR "apoptosis" OR "ovarian follicle atresia" OR "programmed cell death, neutrophils" OR "programmed cell death, B cells" OR "programmed cell death, B lymphocytes" OR "programmed cell"

  Showing Results 1 - 10 of 1,635,000)

  Apoptosis: 3,251,000

  apoptosis site:ncbi.nlm.nih.gov pubmed : 2060


  pubmed:

  PCD: expanded : 430358 (pmed) 2193 (omim)
  PCD: plain, quoted : 9757  (pmed) 95 (omim)
  
  
  **/

has_no_substring_in(_Term,[]):- !.
has_no_substring_in(Term,[Term|L]):- !, has_no_substring_in(Term,L).
has_no_substring_in(Term,[Term2|L]):-
        !,
        \+ sub_atom(Term,_,_,_,Term2),
        has_no_substring_in(Term,L).

replace_search_term_atom(Label,TermAtom):-
        replace_search_term_atom(Label,google,TermAtom).
replace_search_term_atom(Label,Service,TermAtom):-
        entity_label_or_synonym(ID,Label), % TODO - all
        create_search_term_atom(ID,Service,TermAtom).

create_search_term_atom(ID,TermAtom):-
        create_search_term_atom(ID,google,TermAtom).

create_search_term_atom(ID,pubmed,TermAtom):-
        create_search_term_list(ID,Terms),
        maplist(webquote,Terms,QuotedTerms),
        maplist(add_suffix('[All Fields]'),QuotedTerms,SuffixedTerms),
        concat_atom(SuffixedTerms,' OR ',TermAtom1),
        sformat(TermAtom,'(~w)',[TermAtom1]).

create_search_term_atom(ID,_,TermAtom):-
        create_search_term_list(ID,Terms),
        maplist(webquote,Terms,QuotedTerms),
        concat_atom(QuotedTerms,' OR ',TermAtom).

create_search_term_list(ID,Terms):-
        solutions(Term,(subclassRT(CID,ID),entity_label_or_synonym(CID,Term)),AllTerms),
        solutions(Term,(member(Term,AllTerms),has_no_substring_in(Term,AllTerms)),Terms).

webquote(A,AQ):-
        sformat(AQ,'"~w"',[A]).

add_suffix(Suffix,Atom,New):- atom_concat(Atom,Suffix,New).

unittest(test(search,
            [],
            (   ensure_loaded(bio(web_search_expander)),
                ensure_loaded(bio(io)),
                ensure_loaded(bio(ontol_db)),
                load_bioresource(go),
                class(ID,apoptosis),
                create_search_term_atom(ID,Term),
                writeln(Term)),
            true)).

unittest(test(search_phenotype,
            [],
            (   ensure_loaded(bio(web_search_expander)),
                ensure_loaded(bio(io)),
                ensure_loaded(bio(ontol_db)),
                load_bioresource(mammalian_phenotype),
                class(ID,'cardiovascular system phenotype'),
                create_search_term_atom(ID,Term),
                writeln(Term)),
            true)).

unittest(test(search_disease,
            [],
            (   ensure_loaded(bio(web_search_expander)),
                ensure_loaded(bio(io)),
                ensure_loaded(bio(ontol_db)),
                load_bioresource(disease),
                replace_search_term_atom('breast cancer',pubmed,Term),
                writeln(Term)),
            true)).

unittest(test(expand_google,
             [],
             (   ensure_loaded(bio(web_fetch_google)),
                 (   web_fetch_google:disable_apitest(1)
                 ->  true
                 ;   ensure_loaded(bio(io)),
                     ensure_loaded(bio(ontol_db)),
                     load_bioresource(go),
                     replace_search_term_atom(apoptosis,SearchTerm),
                     %sformat(Q,'site:ncbi.nlm.nih.gov AND ~w',[SearchTerm]),
                     %sformat(Q,'~w',[SearchTerm]),
                     %Q=SearchTerm,
                     atom_concat('site:ncbi.nlm.nih.gov ',SearchTerm,Q),
                     writeln(Q),
                     %web_search_google(SearchTerm,Results,[maxResults(10)]),
                     web_search_google(Q,Results,[maxResults(10)]),
                     forall(member(Result,Results),
                            format(' ~w~n~n',[Result])))),
            true)).

/*
expand_search_term(ID,ID-NextID-Pred,ID-Term-Lookup,TermsIn,TermsOut):-
        findall(Term,Lookup,NewTerms),
        findall(Term,(member(Term,NewTerms),has_no_substring_in(Term,TermsIn)),UniqueNewTerms),
        findall(NextID,(Pred,expand_search_term(NextID,X-Y-Pred
        
*/

