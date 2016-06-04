:- module(ontominer,
          [
           direct_concept_token/2,
           concept_enriched/7,
           concept_enriched/3,
           has_differentia/5,
           init_ontominer/0
           ]).

:- use_module(bio(annotator)).
:- use_module(bio(ontol_db)).
:- use_module(bio(bioprolog_util)).
:- use_module(bio(index_util)).

ok_class(C) :-
        class(C),
        id_idspace(C,'GEMET').  % TODO!!!


% REMEMBER: 

init_ontominer :-
        init_ontominer('omine_cache.pro').


init_ontominer(F) :-
        debug(ominfo,'init annotator...',[]),
        initialize_annotator,
        debug(ominfo,'DONE init annotator...',[]),
        materialize_indexes_to_file([
                                     subclassRT(+,+),
                                     direct_concept_token(+,+,-),
                                     concept_token(+,+,-)
                                    ],
                                     F).

/*
%generate_equiv_axiom(C,Genus,Diffs) :-
%        aggregate(max(Score,X),concept_enriched(C,X,Score,subClassOf),max(MaxScore,Genus)),

generate_genus(C,Genus,Pos) :-
        direct_concept_token(C,Genus,Pos),
        subclassT(C,Genus).

generate_equiv_axiom(C,Genus,Diffs) :-
        class(C),
        genus(C,Genus,GenusPos/_),
        solutions(related_to=X,
                  (   direct_concept_token(C,X,XPos/_),
                      XPos\=GenusPis),
                  Diffs),
        Diffs\=[],
        !.
generate_equiv_axiom(C,Genus,Diffs) :-
        class(C),
        direct_concept_token(C,Genus,Pos),
        solutions(related_to=X,
                  (   direct_concept_token(C,X,XPos/_),
                      XPos\=GenusPis),
                  Diffs),
        Diffs\=[],
        !.
*/




%% has_differentia(C,Tok,Score,NumDiffClasses,Differentiae:list)
has_differentia(C,X,Score,NumDiffClasses,Ds) :-
        concept_enriched(C,X,Score,_,_,_,Rel),
        Rel=other_branch,
        solutions(DX,token_subtoken_concept_nonuniq(X,DX,C,_),DXs),
        length(DXs,NumDiffClasses),
        NumDiffClasses > 0,
        solutions(D,token_subtoken_concept_nonuniq(X,_,C,D),Ds).



token_subtoken_concept_nonuniq(X,DX,C,D) :-
        subclassT(DX,X),
        subclassRT(D,C),
        direct_concept_token(D,X,_/Num),
        Num >1.

% todo: learning differentia is DISTINCT
concept_enriched(C,Tok,Score) :-
        concept_enriched(C,Tok,Score,_,_,_,_).
concept_enriched(C,Tok,Score,Rel) :-
        concept_enriched(C,Tok,Score,_,_,_,Rel).

%% concept_enriched(?C,?Tok,?Score,?Precision,?Recall,?CCount,?Rel)
concept_enriched(C,Tok,Score,Precision,Recall,CCount,Rel) :-
        ok_class(C),
        % has at least one child
        \+ \+ subclass(_,C),
        debug(om_info,'TESTING: ~w',[C]),
        concept_token_count(C,Tok,CTCount),  % Relevant-Retrieved documents
        CTCount > 2,  % e.g. must be more than self plus single is-a child
        debug(om,'C: ~w T: ~w',[C,Tok]),
        concept_count(C,CCount),    % Relevant documents
        CCount > 0,
        Recall is CTCount / CCount,
        all_concept_token_count(Tok,TCount),  % Retrieved documents
        debug(om,'   C: ~w T: ~w CT: ~w',[C,Tok,CTCount]),
        TCount > 0,
        Precision is CTCount / TCount,
        Fnum = 2,
        Score is (( 1+Fnum)* Precision * Recall) / (Fnum * Precision + Recall),
        concept_rel(C,Tok,Rel).

concept_rel(C,C,identical) :- !.
concept_rel(C,X,subClassOf) :- subclassT(C,X), !.
concept_rel(C,X,superClassOf) :- subclassT(X,C), !.
concept_rel(_,X,not_in_ontology) :- \+ class(X), !.
concept_rel(_,_,other_branch).




%% concept_token(Class,InferredTokenClass,SubClass)
%
% An instance of a concept-token pair;
% applies to concept and all descendants
% the token may be an unontologized word-token (e.g. "environ")
% or it may be an ontology class (in which case the full closure counts)
concept_token(C,XA,D) :-
        subclassRT(D,C),
        direct_concept_token(D,X),
        subclassRT(X,XA).



%% concept_token_count(?C,?Tok,?Count)
% unifies Tok
concept_token_count(C,Tok,Count) :-
        setof(D,concept_token(C,Tok,D),Ds),
        length(Ds,Count).

% TODO: roots
%% all_concept_token_count(+Tok,?Count)
all_concept_token_count(Tok,Count) :-
        %solutions(Tok,(class(D),direct_concept_token(D,Tok)),Toks),
        setof(D,C^concept_token(C,Tok,D),Toks),  % slow?? todo
        length(Toks,Count).

concept_count(C,Count) :-
        solutions(D,subclassRT(D,C),Ds),
        length(Ds,Count).

%% direct_concept_token(Class,TokenClass,Position/NumMatches)
%                                
% Annotates the label for Class, finds an instance TokenClass label within the label
direct_concept_token(C,Tok) :-
        direct_concept_token(C,Tok,_).
direct_concept_token(C,Tok,Pos/NumMatches) :-
        class(C,Label),
        ok_class(C),
        Opts=[excludes([Label])],
        sentence_annotate(Label,Matches,Opts),
        length(Matches,NumMatches),
        debug(dct,'~w -> ~w',[Label,Matches]),
        nth1(Pos,Matches,Match),
        match_token(Match,Tok).



match_token(Tok,Tok) :-
        atom(Tok),!,
        atom_length(Tok,Len),
        Len > 1.
match_token(m(Cs,_,_),C) :-
        member(C,Cs),
        atom(C),
        !.

        

/*

  Example:

 / GEMET:4279 ! industry
  is_a GEMET:8486 ! timber industry *** 
   is_a GEMET:1245 ! cellulose industry
   is_a GEMET:6830 ! pulp industry
 / GEMET:5086 ! material
  is_a GEMET:5510 ! natural material
   is_a GEMET:3432 ! forest product *** 
    is_a GEMET:1800 ! cork
    is_a GEMET:9348 ! timber
    is_a GEMET:9358 ! wood product
     is_a GEMET:1244 ! cellulose
     is_a GEMET:6828 ! pulp
     is_a GEMET:7458 ! sawdust
     is_a GEMET:8289 ! tannin

this:
  
 / GEMET:5086 ! material
  is_a GEMET:5510 ! natural material
   is_a GEMET:3169 ! fertiliser *** 
    is_a GEMET:11201 ! mineral conditioner
    is_a GEMET:14957 ! semi-liquid manure
    is_a GEMET:5007 ! manure
     is_a GEMET:3766 ! green manure
     is_a GEMET:432 ! animal manure
     is_a GEMET:4843 ! liquid manure
    is_a GEMET:5500 ! natural fertiliser

is disconnected from:

 / GEMET:6660 ! product
  is_a GEMET:11813 ! chemical product
   is_a GEMET:237 ! agrochemical
    is_a GEMET:1307 ! chemical fertiliser *** 
     is_a GEMET:4345 ! inorganic fertiliser
  

suggested template:

%% concept_enriched(Class,Token,Precision,Num)
GEMET:6445 ! pollution  GEMET:6395 ! pollutant  0.4313099041533546      9

GEMET:4350 ! inorganic substance        GEMET:2643 ! chemical element   0.3191489361702128      9

GEMET:2702 ! endangered species  (IUCN) GEMET:7982 ! species    0.29032258064516125     2

GEMET:7986 ! species reintroduction     GEMET:7982 ! species    0.29032258064516125     2


  pollution:

**GEMET:6445 ! pollution  GEMET:6395 ! pollutant  0.4313099041533546      9
GEMET:6445 ! pollution  GEMET:60 ! acoustics    0.07058823529411765     2
GEMET:6445 ! pollution  GEMET:6230 ! physical science   0.06451612903225806     3
*GEMET:6445 ! pollution  GEMET:4124 ! hydrosphere        0.060200668896321065    9
GEMET:6445 ! pollution  GEMET:9232 ! water (geographic) 0.05106382978723405     6
GEMET:6445 ! pollution  GEMET:7472 ! science    0.042628774422735355    8
GEMET:6445 ! pollution  GEMET:1327 ! chemical   0.04067796610169492     3
GEMET:6445 ! pollution  GEMET:6228 ! physical process   0.0375  2
GEMET:6445 ! pollution  GEMET:5116 ! transportation mean        0.0371900826446281      3
GEMET:6445 ! pollution  GEMET:8926 ! vehicle    0.0371900826446281      2
GEMET:6445 ! pollution  GEMET:9051 ! type of waste      0.037037037037037035    2
GEMET:6445 ! pollution  GEMET:2643 ! chemical element   0.036290322580645164    5
GEMET:6445 ! pollution  GEMET:9041 ! waste      0.026865671641791048    3

create: pollution has-pollutant pollutant  
  
  for water pollution (a child):  
  
GEMET:9202 ! water pollution    GEMET:9232 ! water (geographic) 0.24489795918367346     6
GEMET:3493 ! freshwater pollution       GEMET:9232 ! water (geographic) 0.23076923076923073     5
GEMET:9202 ! water pollution    GEMET:4124 ! hydrosphere        0.1592920353982301      9
GEMET:3493 ! freshwater pollution       GEMET:4124 ! hydrosphere        0.11650485436893204     7

  create: water-pollution is-pollution-of hydrosphere

does this change when we add envo? (  hydrosphere has no parents )

  ///
  
  is_a GEMET:7843 ! soil
   is_a GEMET:11233 ! soil salinity
   is_a GEMET:3524 ! soil function
   is_a GEMET:7845 ! soil air
   is_a GEMET:7848 ! soil capability
   is_a GEMET:7882 ! soil profile
   is_a GEMET:7895 ! soil structure
   is_a GEMET:7898 ! soil texture
   is_a GEMET:7899 ! soil type ***
   is_a GEMET:7902 ! soil water
  is_a GEMET:7881 ! soil process
   is_a GEMET:11214 ! purification through the soil
   is_a GEMET:11224 ! soil leaching
   is_a GEMET:7850 ! soil compaction
   is_a GEMET:7858 ! soil erosion
   is_a GEMET:7862 ! soil formation
   is_a GEMET:7867 ! soil loading
   is_a GEMET:7871 ! soil mechanics
   is_a GEMET:7892 ! soil settling
   is_a GEMET:7896 ! soil subsidence


  GEMET:6094 ! pedosphere ENVO:00001998 ! soil    0.6410256410256411      114
  GEMET:7843 ! soil       ENVO:00001998 ! soil    0.4736842105263158      114
  GEMET:7881 ! soil process       ENVO:00001998 ! soil    0.44776119402985076     114

  
  */
