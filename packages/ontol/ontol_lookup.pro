/* -*- Mode: Prolog -*- */

:- module(ontol_lookup,
          [lookup_class/2,
	   lookup_class_by_ontology/2,
	   lookup_property/2]).
:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).
:- use_module(bio(macros_lookup)).

%% query_classes(+QueryTerms:list,?CIDs:list,?Unresolved:list)
% QueryTerm =
%  ids(IDs:list) |
%  names(Names:list) |
%  idfile(FileName)
query_classes(Opts,CIDs,NoIDs) :-
        member(ids(QIDs),Opts),
	nonvar(QIDs),
        QIDs=[_|_],
        !,
        lookup_class_by_ids(QIDs,CIDs,NoIDs).

query_classes(Opts,CIDs,NoIDs) :-
        member(names(QNs),Opts),
	nonvar(QNs),
        QNs=[_|_],
        !,
        lookup_class_by_names(QNs,CIDs,NoIDs). % TODO

query_classes(CIDs,_,CIDs) :- !.

query_classes(CIDs,Opts,NoIDs) :-
        member(idfile(F),Opts),
        nonvar(F),
        !,
        read_file_to_tokens(F,QIDs),
        lookup_class_by_ids(QIDs,CIDs,NoIDs). % TODO


%% lookup_class(+QueryTerm,?Class) is semidet
% @param QueryTerm 
%  QueryTerm =
%   exact(Text) |
%   contains(Match) |
%   begins(Match) |
%   token_search(Match) |
%   search(SearchAtom)
:- lookup_predicate(class,
                    ID,
                    class(ID),
                    [N - class(ID,N),
                     N - entity_synonym(ID,N),
                     N - def(ID,N)]).

:- lookup_predicate(class_by_ontology,
                    ID,
                    class(ID),
                    [N - belongs(ID,N)]).

:- lookup_predicate(property,
                    ID,
                    property(ID),
                    [N - property(ID,N)]).



% -------------------- TESTS --------------------
% regression tests to ensure behaviour of module is correct;
% lines below here are not required for module functionality

unittest(load(sofa)=
      load_biofile(obo,'sofa.obo')/[]).

unittest(test(sofa,
            [_=load(sofa)],
            (   ensure_loaded(bio(ontol_db)),
                ensure_loaded(bio(ontol_lookup)),
                setof(ID,lookup_class(contains(gene),ID),IDs),
                forall(member(ID,IDs),
                       (class(ID,N),writeln(ID-N))),
                nl
                ),
            member('SO:0000336',IDs))).
                 
/** <module> convenient indexing of ontol_db predicates

  ---+ Synopsis

  ==
  :- use_module(bio(io)).
  :- use_module(bio(ontol_lookup)).
  :- use_module(bio(ontol_writer_obo)).

  demo:-
    load_bioresource(go),
    writeln('All terms with "lymphocyte" in name, synonym definition'),
    forall(lookup_class(contains(lymphocyte),ID),
           write_class(obo,ID)).

  ==

  ---+ Description

  see textmatch/3 for possible search terms

  @author Chris Mungall
  @version  $Revision$
  @see README, macros_lookup.pro, textmatch.pro
- 

**/
