:- [bio(obol_obo_xp)]. 

:- multifile process/3,process5/3,term_label/3,continuant/3,term_textdef/3.
:- multifile anatomical_continuant/3.
:- multifile def/3.

term_label(P) --> process(P).

term_textdef(P) --> def(process(P)).


% core
% refered to in xp_quality
regulation(P) --> [regulation],{class_label_exact(P,'biological regulation')}.
% activation of MAPKKK activity
regulation(P) --> any_kind_of(P,'biological regulation').
regulation(P) --> interspecies_modulation(P).

negative_regulation(P) --> [negative],[regulation],{class_label_exact(P,'biological regulation')}.
negative_regulation(P) --> [negative],any_kind_of(P,'biological regulation').
negative_regulation(P) --> [downregulation],{class_label_exact(P,'biological regulation')}.

positive_regulation(P) --> [positive],[regulation],{class_label_exact(P,'biological regulation')}.
positive_regulation(P) --> [positive],any_kind_of(P,'biological regulation').
positive_regulation(P) --> [upregulation],{class_label_exact(P,'biological regulation')}.

% AFAIK this is a synonym for regulation; used exclusively in MOPs?
interspecies_modulation(P) --> [modulation],{class_label_exact(P,'interspecies interaction between organisms')}. % TODO

% alpha-beta T cell activation by superantigen (redundancy with regulation? see xp_self)
process(P that mediated_by(C)) --> process5(P),[by],terminal(C).

% syncytium formation by mitosis without cell division
process(P that preceded_by(P1) and lacks_part(P2)) --> process5(P),[by],process(P1),[without],process(P2).
process(P that preceded_by(P1) and has_part(P2)) --> process5(P),[by],process(P1),[with],process(P2).


% LEVEL 0

% processes taking place in continuants
def(P that occurs_in(C)) --> ['A'],process(P),['in a'],continuant(C).

process(P that occurs_in(C)) --> process5(P),[in],[the],continuant(C).
process(P that occurs_in(C)) --> process5(P),[in],continuant(C). % TODO: opt

% chromatin silencing at centromere
% check this one!! do we want something more direct; eg affects centromere
process(P that unfolds_around(C)) --> process5(P),[at],continuant(C).

% corticospinal neuron axon neuron guidance through the cerebral cortex
process(P that occurs_in(C)) --> process5(P),[through,the],continuant(C). % TODO: opt

% replicative cell ageing
process(P that part_of(P2)) --> process_relational_adj(P2),process(P).
process_relational_adj(P) --> [Adj],{relational_adj_ra(Adj,Noun,biological_process),class(P,Noun)}.

% type I hypersensitivity mediated by mast cells;
process(P that mediated_by(C)) --> process5(P),[mediated],[by],continuant(C).
% glucocorticoid mediated signaling
process5(P that mediated_by(C)) --> continuant(C),[mediated],process5(P).


% LEVEL 5

% bp x continuant [todo: move?]
process5(P that results_in_formation_of(C)) --> continuant(C),creation(P).
creation(P) --> any_kind_of(P,'biosynthetic process').
creation(P) --> [generation],{class_label_exact(P,'biological_process')}. % TODO: check - not all generations are biosynthetic

% peptidyl-glycine cholesteryl ester biosynthesis from peptidyl-glycine
process5(P that results_in_formation_of(C) and has_input(Input)) --> continuant(C),creation(P),[from],continuant(Input).



% apoptotic cell clearance
% sperm nuclear envelope removal
process5(P that results_in_removal_of(C)) --> continuant(C),removal(P).
process5(P that results_in_removal_of(C)) --> removal(P),[of],continuant(C).
removal(P) --> [removal],{class_label_exact(P,'biological_process')}.
removal(P) --> [clearance],{class_label_exact(P,'biological_process')}.

% mRNA modification
% actin modification : Covalent modification of an actin molecule
% peptidyl-tryptophan modification : The chemical alteration of a tryptophan residue in a peptide.
% TODO: is this always covalent? pretend no for now
process5(P that results_in_change_to(C)) --> continuant(C),[modification],{class_label_exact(P,'metabolic process')}.

% insulin processing : The formation of mature insulin by proteolysis of the precursor preproinsulin
% snoRNA processing : Any process involved in the conversion of a primary small nucleolar RNA (snoRNA) transcript into a mature snoRNA.
% TODO: accurately define difference between processing and modification
process5(P that results_in_change_to(C)) --> continuant(C),[processing],{class_label_exact(P,'metabolic process')}.


% POLARIZATION : TODO!!
% lipid raft polarization
% membrane hyperpolarization

% for defs, see more specific differentia

% TRANSPORT
%  the nouns in terms that use the word 'transport' can play different roles depending on their context

% X to Y transport :: X and Y are LOCATIONS (contrast to X transport)
% plasma membrane to endosome transport
process5(P that results_in_transport_from(X) and results_in_transport_to(Y)) --> continuant(X),[to],continuant(Y),transport(P).
% epinephrine transport
% todo: exclude eg plasma membrane transport
process5(P that results_in_transport_of(X)) --> transportable_continuant(X),transport(P). 
process5(P that results_in_transport_of(X) and occurs_in('CL:0000001')) --> [intracellular],continuant(X),transport(P).
% export from nucleus
process5(P that results_in_transport_from(X)) --> [export],[from],continuant(X),{class_label_exact(P,transport)}.
% import into nucleus
process5(P that results_in_transport_to(X)) --> [import],[into],continuant(X),{class_label_exact(P,transport)}.
% protein targeting to Golgi
process5(P that results_in_transport_to(X)) --> transport(P),[to],continuant(X).
% calcium ion transport into cytosol
process5(P that results_in_transport_to(X)) --> transport(P),[into],continuant(X).
% cytoplasmic transport, nurse cell to oocyte
process5(P that results_in_transport_from(X) and results_in_transport_to(Y)) --> transport(P),[','],continuant(X),['to'],continuant(Y).
% mitochondrion transport along microtubule
process5(P that results_in_transport_of(X) and results_in_transport_along(Y)) --> continuant(X),transport(P),['along'],continuant(Y).

% X transport - see also chem, cell, .. for context
transport(P) --> any_kind_of(P,transport).

transportable_continuant(C) --> chemical(C).
transportable_continuant(C) --> cellular_component(C).

/*
  http://www.geneontology.org/GO.process.guidelines.shtml#tpt

  

x localization
[p] establishment of x localization
---[i] establishment of x orientation
---[i] x movement
---[i] x secretion
---[i] x transport
------[i] x export
------[i] x import
[p] maintenance of x localization
---[i] sequestering of x

*/


% release of cytochrome c from mitochondria
process5(P that results_in_release_of(X) and occurs_in(Y)) --> [release],[of],continuant(X),[from],continuant(Y),{class_label_exact(P,biological_process)}.
% GPI anchor release [todo: move to bp x c?]
% Q: what does 'anchor' here denote?
process5(P that results_in_release_of(X)) --> continuant(X),[release],{class_label_exact(P,biological_process)}.

% antibody-dependent cellular cytotoxicity
process5(P that dependent_on(C)) --> terminal(C),['-'],[dependent],process(P).

% TODO!!!
%process5(P that results_in_creation_or_release_of(C)) --> continuant(C),production(P).
%production(P) --> any_kind_of(P,'cytokine production'). % see cell
%production(P) --> any_kind_of(P,'production of molecular mediator of immune response').

% peptide hormone secretion : The regulated release of a peptide hormone from secretory granules. [does granules need to be part of def?]
process5(P that results_in_release_of(C)) --> continuant(C),secretion(P).
secretion(P) --> any_kind_of(P,'secretion').
def(process5(P that results_in_release_of(C))) --> 
        ['regulated release of a'],
        continuant(C),
        ['A kind of'],
        secretion(P).


% PATTERN SPECIFICATION AND DETERMINATION
% axis specification : The establishment, maintenance and elaboration of a pattern along a line or a point
% oocyte axis determination : The establishment, maintenance and elaboration of an axis in the oocyte
% (do we need this: covered by occurs_in pattern)
% see http://sourceforge.net/tracker/index.php?func=detail&aid=1757150&group_id=36855&atid=440764
process5(P that results_in_specification_of(C)) --> continuant(C),patterning(P).
patterning(P) --> any_kind_of(P,'pattern specification process').
% trichome patterning : 
patterning(P) --> [patterning],{class_label_exact(P,'pattern specification process')}.

% BH2 domain binding : todo : need protein ontology
% see also xp_cell - for cell specific bindings
process5(P that results_in_binding_of(X)) --> [binding],[of],continuant(X),{class_label_exact(P,'biological_process')}.

% Myf5 binding : Interacting selectively with the muscle regulatory factor Myf5.
def(process5(_ that results_in_binding_of(X))) -->
    ['Interacting selectively with a'],
    continuant(X).

% nuclear pore localization
process5(P that results_in_localization_of(C)) --> continuant(C),localization(P).
process5(P that results_in_localization_of(C)) --> localization(P),[of],continuant(C).
localization(P) --> any_kind_of(P,localization).

% clustering of voltage-gated potassium channels : The process by which voltage gated potassium channels become localized together in high densities
localization(P) --> [clustering],{class_label_exact(P,localization)}. % todo: make this more specific?


% The processes by which nuclear pores are transported to, or maintained in, a specific location.
% TODO: compare with "maintenance of localization of..."
def(process5(_P that results_in_localization_of(C))) -->
        ['A process by which a'],
        continuant(C),
        ['is transported to, or maintained in, a specific location;'].

% heart contraction
% we assume here that only muscles contract -- WRONG
% process5(P that results_in_contraction_of(C)) --> continuant(C),[contraction],{class_label_exact(P,'biological_process')}.
%% process5(P that results_in_change_to(C)) --> continuant(C),[contraction],{class_label_exact(P,'muscle contraction')}.

% this was moved from bp_xp_chemical: may be 
% lactose catabolic process, using glucoside 3-dehydrogenase
% magnetoreception, using chemical stimulus
process(P that has_enabler(CE)) --> process5(P),[','],[using],molecular_function(CE). % see also: chebi

process(P that has_part(F)) --> process5(P),[via],molecular_function(F).


