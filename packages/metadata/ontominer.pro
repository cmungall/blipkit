:- module(ontominer,
          [
           concept_enriched/3
           ]).

:- use_module(bio(annotator)).
:- use_module(bio(ontol_db)).
:- use_module(bio(bioprolog_util)).

% REMEMBER: initialize_annotator

% todo: learning differentia is DISTINCT
concept_enriched(C,Tok,Score) :-
        class(C),
        % has at least one child
        \+ \+ subclass(_,C),
        concept_token_count(C,Tok,CTCount),  % Retrieved documents
        debug(om,'C: ~w T: ~w',[C,Tok]),
        concept_count(C,CCount),    % Relevant documents
        CCount > 0,
        Recall is CTCount / CCount,
        all_concept_token_count(Tok,TCount),
        debug(om,'   C: ~w T: ~w CT: ~w',[C,Tok,CTCount]),
        TCount > 0,
        Precision is CTCount / TCount,
        Score is Recall * Precision.

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
all_concept_token_count(Tok,Count) :-
        solutions(Tok,(class(D),direct_concept_token(D,Tok)),Toks),
        length(Toks,Count).

concept_count(C,Count) :-
        solutions(D,subclassRT(D,C),Ds),
        length(Ds,Count).

direct_concept_token(C,Tok) :-
        class(C,Label),
        Opts=[excludes([Label])],
        sentence_annotate(Label,Matches,Opts),
        member(Match,Matches),
        match_token(Match,Tok).



match_token(Tok,Tok) :-
        atom(Tok),!,
        atom_length(Tok,Len),
        Len > 1.
match_token(m(Cs,_,_),C) :-
        member(C,Cs),
        atom(C),
        !.

        

        
%learn_concept(C) :-
        
