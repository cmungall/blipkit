/* -*- Mode: Prolog -*- */
:- module(curation_bridge_to_ontol_core,
          [
           curation2term/2
          ]).

:- use_module(bio(curation_db)).
:- use_module(bio(ontol_db)).
:- use_module(bio(bioprolog_util)).

% TODO: evidence??

% link between manifestation and influences stmt
% 1-to-many (curation can make multiple statements)
curation2term(A,restriction(Subj,Rel,Obj)):-
        curation_statement(A,Subj,Rel,Obj),
        class(Subj).
%        class(Obj).

curation2term(A,inst_rel(Subj,Rel,Obj)):-
        curation_statement(A,Subj,Rel,Obj),
        \+ class(Subj).

%        \+ (   class(Subj),
%               class(Obj)).     % or shall we make curation_statement/4 have an extra arg?

ontol_db:restriction(Subj,R,Obj):-
        curation2term(_,restriction(Subj,R,Obj)).
ontol_db:inst_rel(Subj,R,Obj):-
        curation2term(_,inst_rel(Subj,R,Obj)).
        
