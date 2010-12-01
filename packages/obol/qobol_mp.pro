:- use_module(library(thea2/owl2_model),[]).
:- use_module(library(thea2/owl2_plsyn)).
:- use_module(bio(io)).
:- use_module(bio(metadata_nlp)).
:- use_module(bio(ontol_db),[]).
:- use_module(bio(ontol_writer_obo)).

prep :-
        load_bioresource(obol_av).

prep_mp :-
        prep,
        load_bioresource(pato),
        load_bioresource(mammalian_phenotype),
        load_bioresource(mammalian_phenotype_xp).

prep_mp_all :-
        prep_mp,
        load_bioresource(chebi),
        load_bioresource(mouse_anatomy).

%% qobol(?Categories:list, ?MatchTemplate:list, ?OWLExpression, ?MatchGoal, ?ValidGoal)

% ----------------------------------------
% METABOLISM
% ----------------------------------------

qobol([mp,metabolic,level,abnormal],
      [abnormal,circulating,C,level/levels],
      concentration and (qualifier some abnormal) and ( inheres_in some blood) and (towards some C),
      true,
      in(C,_)).

qobol([mp,metabolic,level,abnormal],
      [abnormal,E,C,level/levels],
      concentration and (qualifier some abnormal) and (inheres_in some EX) and (towards some C),
      in(E,'MA',EX),
      in(C,_)).

qobol([mp,metabolic,level,abnormal],
      [abnormal,C,level/levels],
      concentration and (qualifier some abnormal) and (towards some C),
      true,
      in(C,_)).

qobol([mp,metabolic,level,changed],
      [IncOrDec,circulating,C,level/levels],
      Q and ( inheres_in some blood) and (towards some C),
      true,
      in(C,_)) :-
        pato(concentration,IncOrDec,Q).


qobol([mp,metabolic,level,changed],
      [IncOrDec,E,C,level/levels],
      Q and (inheres_in some EX) and (towards some C),
      in(E,'MA',EX),
      in(C,_)) :-
        pato(concentration,IncOrDec,Q).


qobol([mp,metabolic,level,changed],
      [IncOrDec,C,level/levels],
      Q and (towards some C),
      true,
      in(C,_)) :-
        pato(concentration,IncOrDec,Q).

pato(concentration,increased,'increased concentration').
pato(concentration,decreased,'increased concentration').


class_category(C,mp) :- id_idspace(C,'MP').
class_category(C,metabolic) :- class(P,'homeostasis/metabolism phenotype'),subclassRT(C,P).


write_new_parsed_expressions :-
        entity_new_parsed_expression(E,_,X,pass(_)),
        owl2cdef(X,CDef),
        write_cdef(obo,E,CDef),
        fail.


entity_new_parsed_expression(E,Label,X,Msg) :-
        entity_parsed_expression(E,Label,X,Msg,[undefined_only(true)]).
entity_parsed_expression(E,Label,X,Msg,Opts) :-
        ontol_db:class(E),
        parse_entity(E,Label,X,Msg,Opts).

%% parse_entity(+E,?X)
parse_entity(E,_,_,_,Opts) :-
        member(undefined_only(true),Opts),
        genus(E,_),
        !,
        fail.
parse_entity(E,Label,X_Repl,Msg,Opts) :-
        qobol(CatTags,Toks,X,MatchGoal,ValidGoal),
        category_match(CatTags,Opts),
        entity_label_scope(E,Label,_Sc),
        label_template_match(Label,Toks),
        MatchGoal,
        !,
        (   ValidGoal
        ->  Msg=pass(ValidGoal)
        ;   Msg=fail(ValidGoal)),
        expr_repl_labels(X,X_Repl).

category_match(CatTags,Opts) :-
        member(tags(InTags),Opts),
        !,
        forall(member(Tag,InTags),member(Tag,CatTags)).
category_match(_,_).


in(L,Ont) :- in(L,Ont,_).
in(L,Ont,E) :- label_lexical_variant(L,LV),entity_label_scope(E,LV,_),id_idspace(E,Ont).

repl_label(X,X) :- var(X),!.
repl_label(X,X) :- ontol_db:class(X),!.
repl_label(X,X) :- ontol_db:property(X),!.
repl_label(L,X) :- entity_label(X,L),!.
repl_label(L,X) :- entity_label_scope(X,L,_),!.
repl_label(L,X) :- label_lexical_variant(L,LV),LV\=L,entity_label_scope(X,LV,_),!.
repl_label(L,'?'(L)).

expr_repl_labels(In,In) :- var(In),!.
expr_repl_labels(In,Out) :- atom(In),!,repl_label(In,Out).
expr_repl_labels(X and Y,X2 and Y2) :- !,expr_repl_labels(X,X2),expr_repl_labels(Y,Y2).
expr_repl_labels(R some Y,R2 some Y2) :- !,expr_repl_labels(R,R2),expr_repl_labels(Y,Y2).
expr_repl_labels(X,X) :- print_message(error,no_parse(X)).

named_term(X) :- atom(X).
named_term('?'(_)).


owl2cdef(X and Y,cdef(X,L)) :- named_term(X),!,owl2cdef(Y,cdef(X,L)).
owl2cdef(X and Y,cdef(X,L)) :- named_term(Y),!,owl2cdef(X,cdef(X,L)).
owl2cdef(X and Y,cdef(G,L)) :- owl2cdef(X,cdef(G,L1)),owl2cdef(Y,cdef(G,L2)),append(L1,L2,L).
owl2cdef(R some Y,cdef(_,[R=Y])).









