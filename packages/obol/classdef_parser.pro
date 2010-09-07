/* -*- Mode: Prolog -*- */
/**
**/


:- module(classdef_parser,
          [preprocess_tokens/0,
           preprocess_tokens/1,
           normalize_cdef/2,
           name_to_cdef_via_cfg/2,
           name_to_cdef_via_cfg/3,
           textdef_to_cdef_via_cfg/2,
           cdef_to_name_via_cfg/2,
           cdef_to_textdef_via_cfg/2,
           any_kind_of/4,
           invalid_cdef/2,
           id_cdef_fitness/3,
           cdef_fitness/2,
           serialize_cdef/3,
           op(500,xfy,that),
           op(400,xfy,and)]).
:- op(500,xfy,that).
:- op(400,xfy,and).

% doesn't seem to make much difference..
%:- index(user:terminal(1,1,0)).

:- use_module(tokenizer).
:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).
:- use_module(bio(bioprolog_util)).

preprocess_tokens:-
        preprocess_tokens(pre).

preprocess_tokens(pre):-
        debug(obol,'pre-processing',[]),
        forall(class(Class),preprocess(Class)).


preprocess_tokens(lazy):-
        debug(obol,'using lazy processing of terminals',[]),
        %debug(obol,'asserting: ~w',[(terminal(Class,TokensPlusRest,Rest) :- class_label_exact(Class,Label),tok_atom(Label,Tokens))]),
        user:assert( (terminal(Class,TokensPlusRest,Rest) :-
                        debug(obol_test,'~w',t(Class,TokensPlusRest,Rest)),
                        append(Tokens,Rest,TokensPlusRest),stringify(Tokens,Label),class_label_exact(Class,Label) ) ).


preprocess(Class):-
        % make sure primary names go first: these take precedence in generation
        forall_distinct(class(Class,Label),
                        assert_terminal(Class,Label)),
        forall_distinct((class_label_exact(Class,Label),\+class(Class,Label)),
                        assert_terminal(Class,Label)).

assert_terminal(Class,Label):-
        tok_atom(Label,Tokens),
        append(Tokens,Rest,TokensPlusRest),
        debug(obol_preprocess,'asserting terminal for ~w = ~w',[Class,TokensPlusRest]),
        user:assert(terminal(Class,TokensPlusRest,Rest)).

% the above asserts one prod rule for every class. can we cut this down?
%  instead of
%   terminal(abc,[a,b,c|T],T) -- requires linear search; not indexed on list
%   terminal(abc) -->[a,b,c]) -- requires linear search; not indexed on list
%  why not:
%   terminal(abc) --> [a],tx_bc.
%   tx_bc --> [b],tx_c.
%
%   terminal(abc,[a|X],Y) --> tx_bc(X,Y)
%   tx_bc([b|X],Y) --> tx_c(X,Y).
% we still have as many rules, but they are now indexed better?? perhaps not..
assert_terminal2(Class,Label):-
        tok_atom(Label,Tokens),
        append(Tokens,Rest,TokensPlusRest),
        debug(obol_preprocess,'asserting terminal for ~w = ~w',[Class,TokensPlusRest]),
        user:assert(terminal(Class,TokensPlusRest,Rest)).


name_to_cdef_via_cfg(N,CDef):-
        name_to_cdef_via_cfg(N,CDef,term_label).

name_to_cdef_via_cfg(N,CDef,DCGPred):-
        tok_atom(N,Toks),
        (   var(DCGPred)
        ->  DCGPred=term_label
        ;   true),
        debug(obol,'parsing ~w (using: ~w)',[N-Toks,DCGPred]),
        DCGGoal =.. [DCGPred,CDef1,Toks,[]],
        %blipkit:term_label(CDef1,Toks,[]),
        user:DCGGoal,
        debug(obol,'normalizing ~w',[CDef1]),
        normalize_cdef(CDef1,CDef2),
        debug(obol,'normalized: ~w',[CDef2]),
        contract_cdef(CDef2,CDef),
        debug(obol,'contracted: ~w',[CDef]).

% e.g. catalysis
textdef_to_cdef_via_cfg(TextDef,CDef):-
        sub_atom(TextDef,0,_,1,TextDef1), % remove . TODO - required?
        %tok_atom(TextDef1,Toks,[ws(" ")]),
        tok_atom(TextDef1,Toks),
        debug(obol,'parsing ~w',[Toks]),
        user:term_textdef(CDef1,Toks,[]),
        debug(obol,'normalizing ~w',[CDef1]),
        normalize_cdef(CDef1,CDef2),
        debug(obol,'normalized: ~w',[CDef2]),
        contract_cdef(CDef2,CDef),
        debug(obol,'contracted: ~w',[CDef]).




% note: slight asymmetry in generating names
% we want to be more efficient in parsing
% todo: rename; or make initial arg configurable..?
cdef_to_name_via_cfg(CDef1,N):-
        debug(obol,'converting to syntax: ~w',[CDef1]),
        convert_cdef(CDef1,CDef),
        debug(obol,'generating ~w',[CDef]),
        user:term_label(CDef,Toks,[]),
        debug(obol,'stringifying ~w',[Toks]),
        stringify(Toks,N).

cdef_to_textdef_via_cfg(CDef1,TextDef):-
        debug(obol,'converting to syntax: ~w',[CDef1]),
        convert_cdef(CDef1,CDef),
        debug(obol,'generating ~w',[CDef]),
        user:term_textdef(CDef,Toks,[]),
        debug(obol,'stringifying ~w',[Toks]),
        stringify(Toks,TextDef).

% syntax2term
normalize_cdef(G1 that Diffs1,cdef(G,Diffs)):-
        !,
        normalize_cdef(G1,G),
        normalize_diffs(Diffs1,Diffs).
normalize_cdef(( G1 that D1) and D2,cdef(G,Diffs)):-
        !,
        normalize_diffs(D1,DN1),
        normalize_diffs(D2,DN2),
        normalize_cdef(G1,G),
        append(DN1,DN2,Diffs).
normalize_cdef(C,C):- !.

normalize_diffs(Diffs1 and Diffs2,DL):-
        Diffs1 = (_ and _),
        !,
        normalize_diffs(Diffs1,DL1),
        normalize_diffs(Diffs2,DL2),
        append(DL1,DL2,DL).
normalize_diffs(D1 and Diffs1,[D|Diffs]):-
        !,
        normalize_diffs(Diffs1,Diffs),
        normalize_diff(D1,D).
normalize_diffs(D1,[D]):-
        !,
        normalize_diff(D1,D).

normalize_diff(D1,R=To):-
        D1=..[RName,To1],
        !,
        normalize_cdef(To1,To),
        (   property(R,RName)
        ->  true
        ;   %format(user_error,'! No relation defined: ~w~n',[RName]),
            R=RName).
normalize_diff(D1,card(R,Card)=To):-
        D1=..[RName,Card,To1],
        !,
        normalize_cdef(To1,To),
        (   property(R,RName)
        ->  true
        ;   R=RName).
%normalize_diff(D1,R=To/Card):-
%        D1=..[RName,To1,Card],
%        !,
%        normalize_cdef(To1,To),
%        (   property(R,RName)
%        ->  true
%        ;   R=RName).

%% contract_cdef(+CDef,?CDefOut) is det
% if this is a nested cdef, and one of the nested cdefs can be proved equivalent
% to an existing named class, replace the cdef with this named class.
contract_cdef(CDef,CDefOut):-
        contract_cdef(true,CDef,CDefOut),
        !.
contract_cdef(_,R=X,R=X2):-
        !,
        contract_cdef(false,X,X2).
contract_cdef(false,cdef(G,Diffs),X):-
                                % note: use ontol_reasoner for a true test of equivalence.
                                % this just tests for equiavelent N+S conditions
        genus(X,G),
        forall(member(R=To,Diffs),
               differentium(X,R,To)),
        forall(differentium(X,R,To), % check reciprocal
               member(R=To,Diffs)),
        !.
contract_cdef(_,cdef(G,Diffs),cdef(G2,Diffs2)):-
        !,
        contract_cdef(false,G,G2),
        maplist(contract_cdef(false),Diffs,Diffs2).
contract_cdef(_,X,X):- !.



% term2syntax
convert_cdef(cdef(G,Diffs),G1 that Diffs1):-
        !,
        convert_cdef(G,G1),
        convert_diffs(Diffs,Diffs1).
convert_cdef(C,C1) :-
        % recursive expansion - if the referenced class is neither expression nor named,
        % see if there is a logical definition and use this if present
        \+ entity_label(C,_),
        class_cdef(C,CDef),
        !,
        convert_cdef(CDef,C1).
convert_cdef(C,C).

convert_diffs([D],D1):-
        !,
        convert_diff(D,D1).
convert_diffs([D|Diffs],D1 and Diffs1):-
        !,
        convert_diff(D,D1),
        convert_diffs(Diffs,Diffs1).

convert_diff(card(R,Q)=To,D1):-
        convert_cdef(To,To1),
        (   entity_label(R,RName)
        ->  true
        ;   R=RName),
        D1=..[RName,Q,To1].
convert_diff(R=To,D1):-
        convert_cdef(To,To1),
        (   entity_label(R,RName)
        ->  true
        ;   R=RName),
        D1=..[RName,To1].

any_kind_of(SubClass,Label) --> user:terminal(SubClass),{class_label_exact(SuperClass,Label),subclassRT(SubClass,SuperClass)}.


%kind_of(SuperClass,Label) --> terminal(SubClass,Label),{subclassRT(SubClass,SuperClass)}.
%descendant_of(SuperClass,Label) --> terminal(SubClass,Label),{parentRT(SubClass,SuperClass)}.
%descendant_of(SuperClass,SuperClass) --> terminal(SubClass),{parentRT(SubClass,SuperClass)}.

:- multifile id_cdef_fitness_hook/3.

id_cdef_fitness(ID,CDef,S) :-
        id_cdef_fitness_hook(ID,CDef,S),
        !.

id_cdef_fitness(ID,CDef,S) :-
        cdef_fitness(CDef,S1),
	(   CDef=cdef(G,_),
	    subclassT(ID,G)
	->  S is S1+3
	;   S=S1).


% main purpose is to penalise anon classes
cdef_fitness(CDef,S):-
        cdef_fitness(CDef,S,0).
%cdef_fitness(C,S,Depth):-
%        cdef_fitness_hook(C,S,Depth).
cdef_fitness(cdef(G,Diffs),S,Depth):-
        !,
        Depth2 is Depth+1,
        sumof(X,
                (   cdef_fitness(G,X,Depth2)
                ;   member(_=To,Diffs),cdef_fitness(To,X,Depth2)),
              S1),
        (   compound(G) % penalise compound genus (cf neural plate morphogenesis)
        ->  S is S1-1
        ;   S=S1).

% long terms are bad (why? because they are more likely to be spurious constructs like "accumulation_of_oxidatively_modified_proteins_during"),
% but multiple short anons are worse
cdef_fitness(anon(N,_),NegLen,_):-
        !,
        atom_length(N,Len),
        NegLen is -(10-log(Len+1)). %% ENCOURAGE LONG PHRASES
cdef_fitness(_,S,Depth) :-
        S is 0-Depth.

/*
  penalising long words may be wrong:

  Bolwig's organ DEV
  compound eye DEV
  epithelial tube formation

  it may be better to treat the longer phrase as atomic and decompose later
  
  ideally we could recursively decompose and check for repeated functional relations..
  
  */


invalid_cdef(ID,cdef(ID,_)).
invalid_cdef(ID,ID).
invalid_cdef(ID,cdef(_,Diffs)):-
        member(_=ID,Diffs).

serialize_cdef(Fmt,CDef,Atom):-
        format_cdef(Fmt,CDef,Tokens,[]),
        concat_atom(Tokens,' ',Atom).

:- discontiguous format_differentia/3, format_differentia/4.
:- discontiguous format_differentium/4.

% abstract syntax grammar 
format_cdef(abstract,cdef(Genus,Diffs)) --> !,format_cdef(abstract,Genus),[that],format_differentia(abstract,Diffs).
format_cdef(abstract,X) --> {class(X,N)},!,[N].
format_cdef(abstract,X) --> [X].
format_differentia(abstract,[Diff]) --> !,format_differentium(abstract,Diff).
format_differentia(abstract,[Diff|Diffs]) --> !,format_differentium(abstract,Diff),[and],format_differentia(abstract,Diffs).
format_differentium(abstract,R=To) --> !,relation(R),['('],format_cdef(abstract,To),[')'].
relation(R) --> [RN],{entity_label(R,RN)},!.
relation(R) --> [R].

format_class(obo,ID,Elts) --> !,['[Term]'],nl,tag(id,ID),nl,format_elements(obo,Elts).
format_elements(obo,[]) --> !,[].
format_elements(obo,[Elt|Elts]) --> !,format_element(obo,Elt),format_elements(obo,Elts).

format_element(obo,cdef(Genus,Diffs)) --> !,['intersection_of: '],format_ref(obo,Genus),nl,format_differentia(obo,Diffs).
format_differentia(obo,[]) --> !.
format_differentia(obo,[D|Diffs]) --> !,format_differentium(obo,D),format_differentia(obo,Diffs).
format_differentium(obo,R=To) --> !,['intersection_of: '],format_ref(obo,R),format_ref(obo,To),nl.

format_ref(obo,X) --> !,[X].
nl --> "\n".

