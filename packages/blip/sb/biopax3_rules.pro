:- module(biopax3_rules,
          [
           bptype/2,
           op(300, xfy, bptype),
           pathway_subpathway_step/3,
           next_stepT/2,
           next_stepRT/2,
           has_subpathway/2,
           op(300, xfy, has_subpathway),
           has_subpathwayT/2,
           op(300, xfy, has_subpathwayT),
           cpe/2,
           op(300, xfy, cpe),
           cpeT/2,
           op(300, xfy, cpeT),
           cpeRT/2,
           op(300, xfy, cpeRT),
           ontxref/2,
           oboxref/2,
           pathway_reaction_output/3,
           reaction_output/2,
           bp_entity_class/2,
           bp_entity_class_lexmatch/2,
           bp_class_correlation/4
           ]).

/** <module> biopax3_rules

  ---+ Synopsis
  
  ==
  :- use_module(bio(biopax3_rules)).

  demo:-
    load_biofile(owl,'Homo sapiens.owl'),
    Name='Purine metabolism',
    P bp_name Name,
    forall(P has_subpathwayT SubP,
           format('Subpathway, transitive: ~w  ~w~n',[SubP])).
  ==

  ---++ TODO

  simulation module

  */

:- use_module(semweb(rdfs)).
:- use_module(biopax3_db).
%:- use_module(biopax3_bridge_from_biopax1).
:- use_module(bio(metadata_db)).
:- use_module(bio(ontol_db)).
:- use_module(bio(macros_transitive)).

:- op(300, xfy, bptype).
X bptype Y:- rdfs_individual_of(X,Y).

%% has_subpathway(?X,?Y)
% includes reactions as well
:- op(300, xfy, has_subpathway).
X has_subpathway Y:- X pathwayOrder Z, Z stepProcess Y.
X has_subpathway Y:- X pathwayComponent Y.


%% has_subpathwayT(?X,?Y)
% transitive has_subpathway/2
:- transitive has_subpathway/2.
:- op(300, xfy, has_subpathwayT).

cpe(A,B):- A components C, C physical_entity B.
:- transitive cpe/2.
:- op(300, xfy, cpeT).
cpeRT(A,A).
cpeRT(A,B):- cpeT(A,B).

pathway_subpathway_step(P,SubP,S):- P pathway_components S, S step_interactions SubP.
%next_step(S1,S2):- P pathway_components S, S step_interactions SubP.

next_stepT(X,Y):- next_step(X,Y).
next_stepT(X,Y):- next_step(X,Z),next_stepT(Z,Y).

next_stepRT(X,X).
next_stepRT(X,Y):- next_stepT(X,Y).

pathway_reaction_output(P,R,ChC):-
        reaction_output(R,ChC),
        has_subpathwayT(P,R).
reaction_output(R,ChC):-
        R right PEP,
        PEP physical_entity Mol,
        oboxref(Mol,ChC),
        belongs(ChC,chebi_ontology).


rrr_reaction_output(R,ChC):-
        belongs(ChC,chebi_ontology),
        oboxref(X,ChC),
        Mol xref X,
        PEP physical_entity Mol,
        R right PEP.

subpathway_of(SubP,P,0,Step):- Step step_interactions SubP, P pathway_components Step.
subpathway_of(SubP,P,Num2,Step2):- subpathway_of(_,P,Num,Step), Step next_step Step2, Step2 step_interactions SubP,Num2 is Num+1.

oboxref(E,X):-
	E xref ID, ID id Local, atom_concat('MetaCyc:',Local,MetaCyc),entity_xref(X,MetaCyc). % MetaCyc


oboxref(E,X):-
        E xref ID, ID id Local, ID db DB,
        dbsyn(DB,DBSyn),
        concat_atom([DBSyn,':',Local],X).

% grrr
dbsyn('ChEBI','CHEBI').
dbsyn(X,X).

ontxref(E,Class):-
        oboxref(E,Class),
        class(Class).

bp_entity_class_lexmatch(E,C):-
        E bp_name N,
        match_name(N,C).

match_name(N,C):-
        entity_label_or_synonym(C,N).
match_name(N,C):-
        downcase_atom(N,Nd),
        entity_label_or_synonym(C,Nd).

bp_entity_class(E,Class):-
        oboxref(E,Class),
        class(Class).

bp_class_correlation(C1,C2,C1N,C2N):-
        pathway(P),
        bp_entity_class(P,C1),
        has_subpathwayRT(P,SubP),
        rdf_has(SubP,_,E),
        bp_entity_class(E,C2),
        class(C1,C1N),
        class(C2,C2N).

