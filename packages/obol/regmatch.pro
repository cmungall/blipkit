/* -*- Mode: Prolog -*- */
/**
  @author Chris Mungall
  @version @cvskw $Revision: 1.4 $
  @date @cvskw $Date: 2007/06/09 21:45:23 $
  @license @link(url='http://www.fsf.org/licensing/licenses/lgpl.html')|LGPL|

  @s1|Name| regmatch - simple regular grammar matches

  @s1 Synopsis

  @cl
  :- use_module(regmatchname).

  @/cl

  @s1 Description

  DEPRECATED?
  
**/


:- module(regmatch,
          [regmatch_name_to_cdef/2,
           store_cdef/3]).
:- use_module(tokenizer).
:- use_module(bio(mode)).
:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).

:- multifile rpattern/5.
rpattern(_,_,_,_,_):- fail.

%% rpattern(?Sentence,?Template,?TemplateClass,?ClassDef,?Preconditions)
%:- [regmatch_obo_regulation].   %  add your own here..

regmatch_name_to_cdef(S,CDef):-
        %tok_atom(S,SentenceTokens),
        concat_atom(SentenceTokens,' ',S),
        rpattern(SentenceTokens,MatchTemplateTokens,MatchClass,CDef,PreconditionGoal),
        %stringify(MatchTemplateTokens,MatchAtom),
        concat_atom(MatchTemplateTokens,' ',MatchAtom),
        (   class(MatchClass,MatchAtom)
        ->  (   PreconditionGoal % only test once MatchClass has been unified
            ->  true
            ;   format(user_error,'Precondition not satisfied: ~w~n',[MatchAtom]),
                fail)
        ;   format(user_error,'Refered to class does not exist: ~w~n',[MatchAtom]),
            fail).

:- multifile dcg_cdef/3.

%regmatch_name_to_cdef(S,CDef):-
%        tok_atom(S,TL),
%        dcg_cdef(CDef,TL,[]).

%        debug(regmatch,'parse(~w)=~w',[S,CDef]).


/**
  @pred store_cdef(+CDef,?ID,?CDefNormalized)

  given a classdef, store or lookup in db, unifying with ID.
  the classdef is mapped to canonical form (eg no skolems, all IDs)
  */
:- mode store_cdef(+,?,?) is det.
store_cdef(C,ID,C2):-
        debug(classbuilder,'store: ~w',C),
        debug(classbuilder,'*** CLASS TRANSFORM ***',[]),
        %simplify_cdef(C,Cm),
        Cm=C,
        debug(classbuilder,'*** REMAPPED ***',[]),
        debug(classbuilder,'remapped cdef is ~w',Cm),
        store_cdef1(Cm,ID),
        prune_cdef(Cm,Cx,ID),
        debug(classbuilder,'ID for ~w is ~w',[Cx,ID]),
        store_cdef_def(Cx,ID,C2).

% (+,?)
%% store_cdef1(C,ID):-
%%        C=cdef(id_synonym(ID,_Prim),[]),
%        !.
%store_cdef1(C,ID):-
%        C=cdef(id_synonym(_GID,Prim),DL),
%        store_cdef1(cdef(Prim),DL),
%        !.
:- mode store_cdef1(+,?) is det.
%% store_cdef1(+C,?ID) is det
store_cdef1(C,ID):-             % no change; prim ID -> ID
        C=cdef(ID,[]),
        class(ID,_),
        !.
store_cdef1(_,ID):-             % ID provided
        nonvar(ID),
        !.
store_cdef1(C,ID):-             % cdef maps to named class
        cdef_ont(C,O),
        cdef_to_name(C,S),
        (   class(ID,S)
        ;   entity_synonym(ID,exact,S)),   % must be exact
        belongs(ID,O),
        !.
% combine these two clauses??
store_cdef1(C,ID):-             % no existing class: create
        cdef_to_name(C,S),
        !,
        % choose first!
        cdef_ont(C,O),
        %gensym(O,ID),
        generate_new_class_id(O,ID),
        store_fact(ontol_db,is_anonymous(ID)),
        %store_fact(ontol_db,class(ID,S)),
        store_fact(ontol_db,class(ID)),
        store_fact(metadata_db,entity_label(ID,S)),
        store_fact(metadata_db,entity_resource(ID,O)).

:- mode store_cdef_def(+,+,?) is det.
% (+CDef,+ID,?CdefNorm)
store_cdef_def(cdef(_,[]),ID,cdef(ID,[])):- % no def - just map class
        !.
store_cdef_def(cdef(G,DL),ID,cdef(GID,DL2)):-
        store_cdef1(cdef(G,[]),GID),
        retractall_fact(ontol_db,genus(ID,_)),
        retractall_fact(ontol_db,differentium(ID,_,_)),
        % normalize and truncate at depth 1
        findall(P=cdef(DID,[]),(member(P=C,DL),
                                store_cdef1(C,DID)),
                DL2),
        length(DL,Len),
        length(DL2,Len), % hacky check for now
        !,
        store_fact(ontol_db,genus(ID,GID)),
        forall(member(P=cdef(DID,_),DL2),
               store_fact(ontol_db,differentium(ID,P,DID))).
store_cdef_def(C,_,C).

:- mode prune_cdef(+,?,+) is det.
% cdef cannot be in terms of itself
prune_cdef(cdef(ID,_),cdef(ID,[]),ID):-
        !.
prune_cdef(X,X,_).

retractall_fact(M,T):-
        retractall(M:T).

store_fact(M,T):-
        debug(classbuilder,'asserting ~w',[M:T]),
        assert(M:T).
