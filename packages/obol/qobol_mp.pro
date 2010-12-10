:- module(qobol_mp,
          [
           parse_entity/5,
           show_class_parse_mismatch/2,
           suggest_term/5,
           qobol_process_all/1,
           write_new_parsed_expressions/0,
           write_new_parsed_expressions/1,
           qobol_prep/0,
           qobol_prep/1,
           qobol_index/1
           ]).


:- use_module(library(thea2/owl2_model),[]).
:- use_module(library(thea2/owl2_plsyn)).
:- use_module(bio(io)).
:- use_module(bio(tabling)).
:- use_module(bio(index_util)).
:- use_module(bio(metadata_nlp)).
:- use_module(bio(metadata_db)).
:- use_module(bio(ontol_db)).
:- use_module(bio(ontol_writer_obo)).

qobol_index(Opts) :-
        \+ member(noindex(true),Opts),
        !,
        materialize_index(metadata_db:entity_label_scope(1,1,-)),
        materialize_index(metadata_db:entity_label_scope_dn(1,1,-)).

qobol_index(_).

qobol_prep :-
        qobol_prep([]).
qobol_prep(Opts) :-
        member(ontology(Ontology),Opts),
        !,
        qobol_prep_ont(Ontology).
qobol_prep(_) :-
        qobol_prep_ont(_).

qobol_prep_ont('MP') :- prep_mp_all,!.
qobol_prep_ont('HP') :- prep_hp_all,!.
qobol_prep_ont('UBERON') :- load_bioresource(uberonp), !.
qobol_prep_ont(_) :- prep_mp_all,!.

prep :-
        load_bioresource(obol_av).

prep_mp :-
        prep,
        load_bioresource(pato),
        load_bioresource(mammalian_phenotype),
        load_bioresource(mammalian_phenotype_xp).

prep_mp_all :-
        prep_mp,
        load_bioresource(go),
        load_bioresource(protein),
        load_bioresource(chebi),
        load_bioresource(cell),
        load_bioresource(uberonp),
        load_bioresource(mouse_anatomy).

prep_hp :-
        prep,
        load_bioresource(pato),
        load_bioresource(human_phenotype),
        load_bioresource(human_phenotype_xp).

prep_hp_all :-
        prep_hp,
        load_bioresource(go),
        load_bioresource(protein),
        load_bioresource(chebi),
        load_bioresource(cell),
        load_bioresource(uberonp),
        load_bioresource(fma_downcase).



%% qobol(?Categories:list, ?MatchTemplate:list, ?OWLExpression, ?MatchGoal, ?ValidGoal)
:- discontiguous qobol/5.

% ----------------------------------------
% METABOLISM
% ----------------------------------------

qobol([mp,metabolic,level,abnormal],
      [abnormal,circulating,C,level/levels],
      'concentration of' and (qualifier some abnormal) and ( inheres_in some blood) and (towards some C),
      true,
      in(C,_)).

qobol([mp,metabolic,level,abnormal],
      [abnormal,E,C,level/levels],
      'concentration of' and (qualifier some abnormal) and (inheres_in some EX) and (towards some C),
      in(E,'MA',EX),
      in(C,_)).

qobol([mp,metabolic,level,abnormal],
      [abnormal,C,level/levels],
      'concentration of' and (qualifier some abnormal) and (towards some C),
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

% number
qobol([mp,metabolic,anatomy,number,changed],
      [IncOrDec,E,C,number],
      Q and (inheres_in some EX) and (towards some C),
      in(E,'MA',EX),
      in(C,_)) :-
        pato(number,IncOrDec,Q).
qobol([mp,metabolic,basic,number,changed],
      [IncOrDec,C,number],
      Q and (towards some C),
      true,
      in(C,_)) :-
        pato(number,IncOrDec,Q).

% abnormal processes and functions
qobol([mp,process,abnormal],
      [abnormal,P],
      quality and (qualifier some abnormal) and (Rel some P),
      (   in(P,'GO',PID),
          (   belongs(PID,biological_process),
              Rel=inheres_in_part_of
          ;   belongs(PID,molecular_function),
              Rel=inheres_in)),
      true).
/*
qobol([mp,process,abnormal],
      [abnormal,P],
      quality and (qualifier some abnormal) and (inheres_in some P),
      (   in(P,'GO',PID),
          (   belongs(PID,biological_process)
          ;   belongs(PID,molecular_function))),
      true).
*/

% ----------------------------------------
% MORPHOLOGY
% ----------------------------------------

qobol([mp,morphology,abnormal],
      [abnormal,C,morphology],
      morphology and (qualifier some abnormal) and (inheres_in some C),
      true,
      true).

qobol([mp,absent,singular],
      [absent,C],
      'lacks all parts of type' and (towards some C),
      true,
      true).
qobol([mp,absent,plural],
      [absent,C+s],
      'lacks all parts of type' and (towards some C),
      true,
      true).

% note: will not catch: abnormal X function
qobol([mp,eav,abnormal],
      [abnormal,C,A],
      A and (qualifier some abnormal) and (inheres_in some C),
      entity_partition(A,attribute_slim),
      in(A,'PATO')).

qobol([mp,magnitude,changed],
      [Mag,C,A],
      Q and (inheres_in some C),
      (   in(A,'PATO',AX),entity_partition(AX,attribute_slim)),
      atomic_list_concat([Mag,A],' ',Q)
     ) :- magdiff(Mag).


% e.g. X atrophied
qobol([mp,eq,processual],
      [E,P],
      Q and (inheres_in some E),
      true,
      true) :-
        process_quality(P,Q).

% e.g. Q E
qobol([mp,eq,generic],
      [Q,E],
      Q and inheres_in some E,
      in(Q,'PATO'),
      true).

% eg coloboma of the iris
qobol([mp,coloboma],
      [coloboma,of,the,A],
      coloboma and  (inheres_in some A),
      true,
      true).


/*
qobol([mp,persistent,embryonic,plural],
      [persistent,C],
      'persistent TODO' and (inheres_in some C),
      true,
      true).
*/

% ----------------------------------------
% HP
% ----------------------------------------

qobol([hp,quality,simple,plural,abnormal],
      [abnormality,of/involving,opt(the),plural(E)],
      quality and (qualifier some abnormal) and (inheres_in some E),
      true,
      true).
qobol([hp,quality,simple,singular,abnormal],
      [abnormality,of/involving,opt(the),E],
      quality and (qualifier some abnormal) and (inheres_in some E),
      true,
      true).
qobol([hp,quality,combined,plural,abnormal],
      [abnormality,of/involving,opt(the),opt_plural(E1),and,opt_plural(E2)],
      quality and (qualifier some abnormal) and (inheres_in some E1) and (inheres_in some E2),
      true,
      true).

% e.g. abnormal size of the Xs
qobol([hp,eav,simple,plural,abnormal],
      [abnormal,A,of,opt(the),plural(E)],
      A and (qualifier some abnormal) and (inheres_in some E),
      label_partition(A,attribute_slim),
      true).
qobol([hp,eav,simple,singular,abnormal],
      [abnormal,A,of,opt(the),E],
      A and (qualifier some abnormal) and (inheres_in some E),
      label_partition(A,attribute_slim),
      true).

qobol([hp,quality,part,plural,abnormal],
      [abnormality,involving/of,opt(the),opt_plural(P),of,the,opt_plural(W)],
      quality and (qualifier some abnormal) and (inheres_in_part_of some W) and (inheres_in some P),
      true,
      true).
qobol([hp,quality,part,plural,abnormal],
      [abnormality,involving/of,opt(the),W,opt_plural(P)],
      quality and (qualifier some abnormal) and (inheres_in_part_of some W) and (inheres_in some P),
      true,
      true).

qobol([hp,eav,part,plural,abnormal],
      [abnormal,A,of,the,opt_plural(P),of,the,opt_plural(W)],
      A and (qualifier some abnormal) and (inheres_in_part_of some W) and (inheres_in some P),
      label_partition(A,attribute_slim),
      true).
qobol([hp,eav,part,plural,abnormal],
      [abnormal,A,of,the,W,opt_plural(P)],
      A and (qualifier some abnormal) and (inheres_in_part_of some W) and (inheres_in some P),
      label_partition(A,attribute_slim),
      true).

% ----------------------------------------
% ANATOMY
% ----------------------------------------
qobol([anatomy,uberon,interdigital],
      [interdigital,region,between,Digit,X,and,Y],
      'interdigital region' and adjacent_to some DX and adjacent_to some DY,
      digit_number(Y),
      (   atomic_list_concat([Name,X],' ',DX),
          atomic_list_concat([Name,Y],' ',DY)
      )
     ) :-
        digit_name(Digit,Name).

digit_name(digits,digit).
digit_name(fingers,'hand digit').
digit_name(toes,'foot digit').
digit_number('1').
digit_number('2').
digit_number('3').
digit_number('4').
digit_number('5').

qobol([anatomy,uberon,subdivision,spine],
      [Adj,X],
      X and part_of some Region, % not quite right...
      true,
      true
     ) :-
        spinal_region(Adj,Region).

spinal_region(cervical,'cervical region').
spinal_region(thoracic,'thoracic region').
spinal_region(lumbar,'lumbar region').
spinal_region(sacral,'sacral region').
spinal_region(caudal,'caudal region').
spinal_region(coccygeal,'caudal region').



% ----------------------------------------
% TEMPLATE LOGIC
% ----------------------------------------

magdiff(increased).
magdiff(decreased).

pato(concentration,increased,'increased concentration').
pato(concentration,decreased,'decreased concentration').

pato(number,increased,'has extra parts of type').
pato(number,decreased,'has fewer parts of type').
pato(number,abnormal,'altered number of').

process_quality(degeneration,degenerate).
process_quality(atrophy,atrophied).


class_category(C,mp) :- id_idspace(C,'MP').
class_category(C,metabolic) :- class(P,'homeostasis/metabolism phenotype'),subclassRT(C,P).

% ----------------------------------------
% OPTION PROCESSING
% ----------------------------------------

opts_excluded_class(E,Opts) :-
        member(ontology(Ont),Opts),
        \+ id_idspace(E,Ont).
opts_excluded_class(E,Opts) :-
        member(undefined_only(true),Opts),
        genus(E,_).
opts_excluded_class(E,Opts) :-
        member(subclass(X),Opts),
        \+ subclassRT(E,X).
opts_excluded_class(E,Opts) :-
        member(id(X),Opts),
        X\=E.

opts_included_class(E,Opts) :-
        \+ opts_excluded_class(E,Opts).

opts_allowed_scope(exact,_) :- !.
opts_allowed_scope(label,_) :- !.
opts_allowed_scope(Scope,Opts) :-
        member(scope(Scope),Opts).


category_match(CatTags,Opts) :-
        member(tags(InTags),Opts),
        !,
        forall(member(Tag,InTags),member(Tag,CatTags)),
        \+ category_exclude(CatTags,Opts).
category_match(CatTags,Opts) :-
        setof(Tag,member(tag(Tag),Opts),InTags),
        !,
        forall(member(Tag,InTags),member(Tag,CatTags)),
        \+ category_exclude(CatTags,Opts).
category_match(CatTags,Opts) :-
        \+ category_exclude(CatTags,Opts).


category_exclude(CatTags,Opts) :-
        member(xtag(Tag),Opts),
        member(Tag,CatTags).


% ----------------------------------------
% USER GOALS
% ----------------------------------------

qobol_process_all(Opts) :-
        ontol_db:class(E),
        qobol_process_class(E,Opts),
        fail.
qobol_process_all(_).

qobol_process_class(E,Opts) :-
        \+ opts_included_class(E,Opts),
        !.
qobol_process_class(E,Opts) :-
        parse_entity(E,X,Msg,Opts),
        !,
        print_message(informational,Msg),
        qobol_process_parse(E,X,Msg,Opts).
qobol_process_class(E,_Opts) :-
        print_message(informational,no_parse(E)).

qobol_process_parse(_,_,fail(_),_) :- !.
qobol_process_parse(E,X,pass(_),Opts) :-
        member(export(obo),Opts),
        !,
        qobol_write_obo(E,X,Opts).
qobol_process_parse(E,X,pass(Msg),Opts) :-
        !,
        print_message(informational,ok(E,Msg,X,Opts)).


qobol_write_obo(E,X,_Opts) :-
        owl2cdef(X,CDef),
        !,
        write_cdef(obo,E,CDef).
qobol_write_obo(E,X,_Opts) :-
        print_message(informational,no_owl(E,X)).

write_new_parsed_expressions :-
        write_new_parsed_expressions([]).
write_new_parsed_expressions(Opts) :-
        entity_parsed_expression(E,_Label,X,Msg,[undefined_only(true)|Opts]),
        print_message(informational,Msg),
        (   Msg=fail(_)
        ->  true
        ;   owl2cdef(X,CDef),   % fails if ?(_)s cannot be resolved
            writeln(cdef=CDef),
            write_cdef(obo,E,CDef)),
        fail.
write_new_parsed_expressions(_).

entity_new_parsed_expression(E,Label,X,Msg) :-
        entity_parsed_expression(E,Label,X,Msg,[undefined_only(true)]).

entity_parsed_expression(E,Label,X,Msg,Opts) :-
        ontol_db:class(E),
        opts_included_class(E,Opts),
        parse_entity(E,Label,X,Msg,Opts).

show_class_parse_mismatch(E,Opts) :-
        uniq_class(E),
        opts_included_class(E,Opts),
        compare_parse(E,_,_,fail(mismatch(_,Def1\=Def2)),Opts),
        writeln('<<<<<<<<'),
        write_cdef(obo,E,Def1),
        writeln('--------'),
        write_cdef(obo,E,Def2),
        writeln('>>>>>>>>'),
        nl.

suggest_term(E,Label,X_Repl,NewTerm,Opts) :-
        uniq_class(E),
        opts_included_class(E,Opts),
        suggest_term_d(E,Label,X_Repl,NewTerm,Opts),
        \+ entity_label_scope(_,NewTerm,_).


% ----------------------------------------
% MAIN LOGIC
% ----------------------------------------

uniq_class(C) :- setof(C,class(C),Cs),member(C,Cs).

%% parse_entity(+E,?Label,?X,?Msg,+Opts:list)
% Label is unified with whichever label was used to gain the expression
parse_entity(E,X,Msg,Opts) :-
        parse_entity(E,_,X,Msg,Opts).
parse_entity(E,Label,X_Repl,Msg,Opts) :-
        select(compare(true),Opts,Opts2),
        !,
        debug(qobol,'Comparing: ~w',[E]),
        compare_parse(E,Label,X_Repl,Msg,Opts2).
parse_entity(E,_,_,_,Opts) :-
        member(undefined_only(true),Opts),
        genus(E,_),
        !,
        print_message(informational,xp_exists_for(E)),
        fail.
parse_entity(E,Label,X_Repl,Msg,Opts) :-
        debug(qobol,'Parsing: ~w',[E]),
        qobol(CatTags,Toks,X,MatchGoal,ValidGoal),
        category_match(CatTags,Opts),
        entity_label_scope(E,Label_1,Sc),
        %label_lexical_variant(Label_1,Label),
        downcase_atom(Label_1,Label),
        debug(qobol,'  Label: ~w',[Label]),
        opts_allowed_scope(Sc,Opts),
        label_template_match(Label,Toks),
        debug(qobol,'  Match: ~w // Testing: ~w',[Toks,MatchGoal]),
        MatchGoal,
        debug(qobol,'  Valid: ~w // ~w',[Toks,MatchGoal]),
        (   ValidGoal
        ->  Msg=pass(ValidGoal)
        ;   Msg=fail(ValidGoal)),
        debug(qobol,'  Msg: ~w // repl(~w)',[Msg,X]),
        expr_repl_labels(X,X_Repl,[force(true)|Opts]), % no ?s
        debug(qobol,'  Repl: ~w --> ~w',[X,X_Repl]),
        !.

suggest_term_d(E,Label,X_Repl,NewTerm,Opts) :-
        debug(qobol,'Parsing: ~w',[E]),
        qobol(CatTags,Toks,X,MatchGoal,ValidGoal),
        category_match(CatTags,Opts),
        entity_label_scope(E,Label_1,Sc),
        label_lexical_variant(Label_1,Label),
        debug(qobol,'  Label: ~w',[Label]),
        opts_allowed_scope(Sc,Opts),
        label_template_match(Label,Toks),
        MatchGoal,
        expr_repl_labels(X,X_Repl,Opts),
        (   expr_unresolved(X_Repl,NewTerm)
        ;   \+ ValidGoal,
            NewTerm=ValidGoal),
        \+ parse_entity(E,_,_,pass(_),[force(true)|Opts]),
        !.



compare_parse(E,_,_,pass(no_existing),_) :-
        \+ genus(E,_),
        !.
compare_parse(E,Label,X_Repl,Msg,Opts) :-
        parse_entity(E,Label,X_Repl,pass(_),Opts),
        !,
        class_cdef(E,CDef),
        match_cdef(E,CDef,X_Repl,Msg).
compare_parse(E,_,_,fail(no_parse(E)),_).

match_cdef(E,CDef,X,Msg) :-
        owl2cdef(X,cdef(G,DL)),
        !,
        sort(DL,DL_Sorted),
        CDef2=cdef(G,DL_Sorted), % parsed
        (   CDef=CDef2
        ->  Msg=pass(matches)
        ;   Msg=fail(mismatch(E,CDef\=CDef2))).
match_cdef(_,_,_,fail(owl2cdef)).



% ----------------------------------------
% EXPRESSION PROCESSING
% ----------------------------------------

label_partition(L,S) :- in(L,_,E),entity_partition(E,S).
in(L,Ont) :- in(L,Ont,_).
in(L,Ont,E) :- label_lexical_variant(L,LV),entity_label_scope(E,LV,_),id_idspace(E,Ont),\+entity_obsolete(E,_).

repl_label(L,X,Opts):-
        repl_label_1(L,X_1,Score,Opts),
        \+ entity_obsolete(X_1,_),
        !,
        (   atom(X_1),
            id_idspace(X_1,'UBERON'),
            repl_label_1(L,X,Alt_Score,Opts),
            atom(X),
            \+ id_idspace(X,'UBERON'),
            Alt_Score >= Score
        ->  true
        ;   X=X_1).

:- initialization(table_pred(repl_label_1/4)).
repl_label_1(X,X,5,_Opts) :- var(X),!.
repl_label_1(X,X,5,_Opts) :- ontol_db:class(X).
repl_label_1(X,X,5,_Opts) :- ontol_db:property(X).
repl_label_1(L,X,4,_Opts) :- entity_label(X,L).
repl_label_1(L,X,Score,Opts) :- entity_label_scope(X,L,Sc),opts_allowed_scope(Sc,Opts),scope_score(Sc,Score).
repl_label_1(L,X,Score,Opts) :- label_lexical_variant(L,LV),LV\=L,entity_label_scope_dn(X,LV,Sc),opts_allowed_scope(Sc,Opts),scope_score(Sc,ScoreFull),Score is ScoreFull-1.
repl_label_1(L,'?'(L),1,Opts) :- \+member(force(true),Opts).

expr_repl_labels(In,In,_Opts) :- var(In),!.
expr_repl_labels(In,Out,Opts) :- atom(In),!,repl_label(In,Out,Opts).
expr_repl_labels(X and Y,X2 and Y2,Opts) :- !,expr_repl_labels(X,X2,Opts),expr_repl_labels(Y,Y2,Opts).
expr_repl_labels(R some Y,R2 some Y2,Opts) :- !,expr_repl_labels(R,R2,Opts),expr_repl_labels(Y,Y2,Opts).
expr_repl_labels(X,X,Opts) :- print_message(error,no_parse(X,Opts)).

named_term(X) :- atom(X).
%named_term('?'(_)).

scope_score(label,3) :- !.
scope_score(exact,3) :- !.
scope_score(_,2) :- !.

expr_unresolved('?'(X),X).
expr_unresolved(X and _,Z) :- expr_unresolved(X,Z).
expr_unresolved(_ and Y,Z) :- expr_unresolved(Y,Z).
expr_unresolved(_ some Y,Z) :- expr_unresolved(Y,Z).



owl2cdef(X and Y,cdef(X,L)) :- named_term(X),!,owl2cdef(Y,cdef(X,L)).
owl2cdef(X and Y,cdef(X,L)) :- named_term(Y),!,owl2cdef(X,cdef(X,L)).
owl2cdef(X and Y,cdef(G,L)) :- owl2cdef(X,cdef(G,L1)),owl2cdef(Y,cdef(G,L2)),append(L1,L2,L).
owl2cdef(R some Y,cdef(_,[R=Y])) :- named_term(Y).






