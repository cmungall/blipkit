:- [bio(obol_biological_process_xp_anatomy)].
:- [bio(obol_biological_process_xp_cellular_component)].

:- multifile process/3,process5/3,anatomical_continuant/3,cell/3.

/*

  See also:
  http://www.geneontology.org/GO.process.guidelines.shtml

x development
[p] x morphogenesis
---[p] x formation
------[p] y cell differentiation
---[p] x structural organization
[p] x maturation

cellular process
[i] cell differentiation
---[p] cell fate commitment
------[p] cell fate specification
------[p] cell fate determination
---[p] cell development
------[p] cellular morphogenesis during differentiation
------[p] cell maturation
  
*/


% histamine secretion by basophil
process(P that mediated_by(Cell)) --> process5(P),[by],cell(Cell).

% +++++ Level5 +++++

% neuroblast division
% The process resulting in the physical partitioning and separation of a neuroblast into daughter cells. A neuroblast is any cell that will divide and give rise to a neuron
process5(P that results_in_division_of(Cell)) --> cell(Cell),division(P).
division(P) --> any_kind_of(P,'cell division').
division(P) --> [division],{class_label_exact(P,'cell division')}.
def(process(P that results_in_division_of(C))) -->
        ['A'],process(P),['resulting in the physical partitioning and separation of a'],continuant(C),
        ['into daughter cells'].

/****************************************

  Cell fate specification, commitment and determination

  TODO:
  
  do we need 3 relations, or is 1 relation plus the genus enough?
  
  ****************************************/

%% ----------------------------------------
%% cell fate specification
%% ----------------------------------------

% todo: abstract these?
process5(P that results_in_specification_of(Cell)) --> cell(Cell),cell_fate_specification(P).
cell_fate_specification(P) --> any_kind_of(P,'cell fate specification').
cell_fate_specification(P) --> terminal(P,'cell fate specification',[specification]).
cell_fate_specification(P) --> [fate,specification],{class_label_exact(P,'cell fate specification')}.

% myoblast cell fate specification :
% The process whereby a cell becomes capable of differentiating autonomously into a myoblast cell in an environment that is neutral with respect to the developmental pathway
def(process(_ that results_in_specification_of(C))) -->
    ['A process whereby a cell becomes capable of differentiating autonomously into a'],
    cell(C),
    ['in an environment that is neutral with respect to the developmental pathway'].
                                 
%% ----------------------------------------
%% cell fate determination : SAME AS SPECIFICATION
%% ----------------------------------------

process5(P that results_in_specification_of(Cell)) --> cell(Cell),cell_fate_determination(P).
cell_fate_determination(P) --> any_kind_of(P,'cell fate determination').
cell_fate_determination(P) --> terminal(P,'cell fate determination',[determination]).
cell_fate_determination(P) --> [fate,determination],{class_label_exact(P,'cell fate determination')}.
cell_fate_determination(P) --> [lineage,determination],{class_label_exact(P,'cell fate determination')}.

% oocyte fate determination :
% Process by which a cell becomes capable of differentiating autonomously into an oocyte cell regardless of its environment; upon determination, the cell fate cannot be reversed.
def(process(_ that results_in_determination_of(C))) -->
        ['A process by which a cell becomes capable of differentiating autonomously into an'],
        cell(C),
        ['regardless of its environment; upon determination, the cell fate cannot be reversed.'].

%% ----------------------------------------
%% cell fate commitment
%% ----------------------------------------

process5(P that results_in_commitment_to(Cell)) --> cell(Cell),cell_fate_commitment(P).
cell_fate_commitment(P) --> any_kind_of(P,'cell fate commitment').
cell_fate_commitment(P) --> terminal(P,'cell fate commitment',[commitment]).
cell_fate_commitment(P) --> [fate,commitment],{class_label_exact(P,'cell fate commitment')}.
cell_fate_commitment(P) --> [lineage,commitment],{class_label_exact(P,'cell fate commitment')}.

% photoreceptor cell fate commitment : The process whereby the developmental fate of a cell becomes restricted such that it will develop into a photoreceptor cell
def(process(P that results_in_commitment_to(C))) -->
        ['A process whereby the developmental fate of a cell becomes restricted such that it will develop into a'],
        cell(C),
        ['; A kind of'],
        cell_fate_commitment(P).  % we need this to check P

/****************************************

  cell activation, binding and transport
  
  ****************************************/

%% ----------------------------------------
%% cell activation
%% ----------------------------------------

process5(P that results_in_change_to(Cell)) --> cell(Cell),cell_activation(P).
cell_activation(P) --> terminal(P,'cell activation',[activation]).

% neutrophil activation : The change in morphology and behavior of a neutrophil resulting from exposure to a cytokine, chemokine, cellular ligand, or soluble factor.
def(process(P that has_input(C))) -->
        ['The change in morphology and behavior of a'],
        cell(C),
        ['resulting from exposure to a cytokine, chemokine, cellular ligand, or soluble factor;'],
        ['A kind of'],
        cell_activation(P).

%% ----------------------------------------
%% cytokine production
%% ----------------------------------------

% B cell cytokine production
process5(P that mediated_by(Cell)) --> cell(Cell),cytokine_production(P).
cytokine_production(P) --> any_kind_of(P,'cytokine production').
def(process(P that mediated_by(Cell))) -->
        ['A process that contributes to'],
        cytokine_production(P),
        ['by a'],
        cell(Cell).

%% ----------------------------------------
%% binding of cells
%% ----------------------------------------
%% see also process_xp

% example: binding of sperm to zona pellucida; 
% todo: QCRs
process5(P that results_in_binding_of(X) and results_in_binding_of(Y)) --> [binding],[of],cell(X),[to],cell(Y),{class_label_exact(P,'cellular process')}.
def(process5(_ that results_in_binding_of(X) and results_in_binding_of(Y))) -->
    ['A process by which a'],
    continuant(X),
    ['binds to a'],
    continuant(Y).

%% ----------------------------------------
%% transport of cells
%% ----------------------------------------

% nurse cell to oocyte transport
process5(P that results_in_transport_from(X) and results_in_transport_to(Y)) --> cell(X),[to],cell(Y),transport(P).

% motility
process5(P that results_in_movement_of(Cell)) --> cell(Cell),cell_motility(P).
cell_motility(P) --> any_kind_of(P,'cell motility').
cell_motility(P) --> [motility],{class_label_exact(P,'cell motility')}.
cell_motility(P) --> [movement],{class_label_exact(P,'cell motility')}.
cell_motility(P) --> [migration],{class_label_exact(P,'cell migration')}. % neural crest cell migration

%% ----------------------------------------
%% delamination
%% ----------------------------------------
delamination(P) --> [delamination],{class_label_exact(P,delamination)}.
% oenocyte delamination
process5(P that occurs_in(C)) --> cell(C),delamination(P).

/****************************************

  apoptosis, programmed cell death and deletion

  immunity and defense
  
  ****************************************/

% nurse cell apoptosis
process5(P that results_in_death_of(Cell)) --> cell(Cell),cell_death(P).
%cell_death(P) --> terminal(P),{class_label_exact(RootP,'cell death'),subclassRT(P,RootP)}.
cell_death(P) --> any_kind_of(P,'cell death').
cell_death(P) --> [death],{class_label_exact(P,'cell death')}. % synergid death

% B cell deletion
process5(Death that results_in_death_of(Cell) and part_of(IP)) --> cell(Cell),[deletion],{class_label_exact(Death,'apoptosis'),class_label_exact(IP,'immune system process')}.

% B cell anergy
process5(P that inactivates(Cell)) --> cell(Cell),[anergy],{class_label_exact(P,'immune system process')}.

% B cell mediated immunity
process5(P that mediated_by(Cell)) --> cell(Cell),immune_system_process(P).
process5(P that mediated_by(Cell)) --> cell(Cell),[mediated],immune_system_process(P).
immune_system_process(P) --> [immunity],{class_label_exact(P,'immune system process')}.
immune_system_process(P) --> any_kind_of(P,'immune system process').

% B cell receptor editing
process5(P that results_in_editing_of(CC that part_of(Cell))) --> cell(Cell),cellular_component(CC),[editing],{class_label_exact(P,biological_process)}.

% B cell receptor V(D)J recombination
process5(P that results_in_editing_of(CC that part_of(Cell))) --> cell(Cell),cellular_component(CC),recomb(P).
recomb(P) --> any_kind_of(P,'V(D)J recombination').

% T cell costimulation
% process of providing, via surface-bound receptor-ligand pairs, a second, antigen-independent, signal in addition to that provided by the T cell receptor to augment T cell activation
%%% process5(P that positively_regulates(ActivationP that results_in_change_to(C))) --> cell(C),[costimulation],{class_label_exact(P,'biological regulation'),class_label_exact(ActivationP,'cell activation')}. too complicated
process5(P that costimulates_activation_of(C)) --> cell(C),[costimulation],{class_label_exact(P,'immune system process')}.

% T cell receptor signaling pathway
% A series of molecular signals initiated by the cross-linking of an antigen receptor on a T cell
process5(P that mediated_by(C)) --> cell(C),[receptor,signaling,pathway],{class_label_exact(P,'antigen receptor-mediated signaling pathway')}.
%process5(P that mediated_by(Receptor that part_of(C))) --> cell(C),[receptor,signaling,pathway],{class_label_exact(P,'antigen receptor-mediated signaling pathway'),class(Receptor,'receptor complex')}.


% B cell selection
process5(P that results_in_selection_of(C)) --> cell(C),[selection],{class_label_exact(P,'biological_process')}.  % ??????
process5(P that results_in_negative_selection_of(C)) --> cell(C),[negative],[selection],{class_label_exact(P,'biological_process')}.  % ??????
process5(P that results_in_positive_selection_of(C)) --> cell(C),[positive],[selection],{class_label_exact(P,'biological_process')}.  % ??????


% T-cell mediated cytotoxicity
process5(P that has_enabler(Cell)) --> cell(Cell),[mediated],cell_killing(P).
cell_killing(P) --> any_kind_of(P,'cell killing').
cell_killing(P) --> [cytotoxicity],{class_label_exact(P,'cell killing')}.

% dendritic cell chemotaxis : The movement of a dendritic cell in response to an external stimulus
process5(P that results_in_movement_of(Cell)) --> cell(Cell),chemotaxis(P).
chemotaxis(P) --> any_kind_of(P,chemotaxis). % TODO: check - eg germ cell attraction
chemotaxis(P) --> negative_chemotaxis(P).
chemotaxis(P) --> positive_chemotaxis(P).
def(process5(P that results_in_movement_of(Cell))) -->
        ['A process that results in the movement of'],
        cell(Cell),
        ['In response to an external stimulus; A kind of'],
        chemotaxis(P).

negative_chemotaxis(P) --> [repulsion],{class_label_exact(P,'negative chemotaxis')}.
positive_chemotaxis(P) --> [attraction],{class_label_exact(P,'positive chemotaxis')}.

process5(P that in_response_to(AgentP)) --> protection(P),[from],process(AgentP).
protection(P) --> [protection],{class_label_exact(P,'cellular defense response')}.

/****************************************

  differentiation
  
  ****************************************/

% oocyte differentiation
process5(P that results_in_acquisition_of_features_of(C)) --> cell(C),cell_differentiation(P).
cell_differentiation(P) --> any_kind_of(P,'cell differentiation').
cell_differentiation(P) --> [differentiation],{class_label_exact(P,'cell differentiation')}.

% Sertoli cell differentiation : The process whereby a relatively unspecialized cell acquires specialized structural and/or functional features of a Sertoli cell
def(process5(_P that results_in_acquisition_of_features_of(C))) -->
        ['A process whereby a relatively unspecialized cell acquires specialized structural and/or functional features of a'],
        cell(C).

% oocyte construction : The synthesis, deposition, and organization of a cell of an animal ovary that can then undergo meiosis and form an ovum
% X construction partof X development; isa X developmental process
% note: we define formation elsewhere. this is specific to cells
process5(P that results_in_formation_of(C)) --> cell(C),[construction],{class_label_exact(P,'anatomical structure formation')}.
def(process5(_P that results_in_formation_of(C))) -->
        ['Synthesis, deposition, and organization of a'],
        cell(C).



% stem cell maintenance : [is_a -ve reg of cell differentiaion]
% The process by which an organism retains a population of stem cells, preventing the commitment of all stem cell progeny to a differentiated cell fate
% photoreceptor cell maintenance : [is_a cellular homeostasis -- equilibrium at the level of the cell]
% Any process preventing the degeneration of the photoreceptor, a specialized cell type that is sensitive to light
%
% 
% TODO: check
% note the two different usages. This is for the former: 
process5(P that negatively_regulates(P2 that results_in_acquisition_of_features_of(C))) -->
        cell(C),[maintenance],{class_label_exact(P,'multicellular organismal process'),class_label_exact(P2,'cell differentiation')}.


%% ----------------------------------------
%% homeostasis and proliferation
%% ----------------------------------------

% T cell proliferation : The rapid expansion of a T cell population by cell division. Follows T cell activation
process5(P that increases_population_size_of(C)) --> cell(C),cell_proliferation(P).
cell_proliferation(P) --> [proliferation],{class_label_exact(P,'cell proliferation')}.
cell_proliferation(P) --> any_kind_of(P,'cell proliferation').
cell_proliferation(P) --> any_kind_of(P,'homeostasis of number of cells').
def(process5(_P that increases_population_size_of(C))) -->
        ['A rapid expansion of a'],cell(C),
        ['population by cell division. Follows'],
        cell(C),['activation'].

% T cell homeostatic proliferation :
%  The non-specific expansion of T cell populations within a whole or part of an organism to reach to a total number
%  of T cells which will then remain stable over time in the absence of an external stimulus
process5(P that increases_population_size_of(C) and part_of(HP)) --> cell(C),[homeostatic],cell_proliferation(P),{class_label_exact(HP,'homeostatic process')}.
def(process5(P that increases_population_size_of(C) and part_of(HP))) -->
        ['A non-specific expansion of'],cell(C),
        ['populations within a whole or part of an organism to reach to a total number of'],
        cell(C),['s which will then remain stable over time in the absence of an external stimulus.'],
        ['A kind of'],cell_proliferation(P),['that is part of'], process(HP).   % gloss

% T cell homeostasis :
%  The process of regulating the proliferation and elimination of T cells such that the total number of T cells
%  within a whole or part of an organism is stable over time in the absence of an outside stimulus
process5(P that regulates_population_size_of(C)) --> cell(C),homeostasis_of_number_of_cells(P),{\+class(C,cell)}.
homeostasis_of_number_of_cells(P) --> [homeostasis],{class_label_exact(P,'homeostasis of number of cells')}.
def(process5(_P that regulates_population_size_of(C))) -->
        ['A process of regulating the proliferation and elimination of'],cell( C),
        ['s such that the total number of'],cell(C),
        ['s within a whole or part of an organism is stable over time in the absence of an outside stimulus'].

% leukocyte degranulation
% process5(P that results_in_release_from(C) and results_in_release_of(Granule)) --> cell(C),degranulation(P),{class_label_exact(Granule,'secretory granule')}.
%process5(P that occurs_in(C)) --> cell(C),degranulation(P).
process5(P that results_in_release_from(C)) --> cell(C),degranulation(P).
degranulation(P) --> [degranulation],{class_label_exact(P,'exocytosis')}.
granule(G) --> terminal(G),{class_label_exact(G,'secretory granule')}.

% basophil degranulation : The regulated exocytosis of secretory granules containing preformed mediators such as histamine, serotonin, and neutral proteases by a basophil
def(process5(_P that results_in_release_from(C) and results_in_release_of(G))) -->
        ['Exocytosis of a'],
        granule(G),
        ['containing preformed mediators by a'],
        cell(C).

% ----
% POLARITY
% ----
% see also xp_quality..

process(P that occurs_in(X)) --> [establishment,of],cell(X),[polarity],{class_label_exact(P,'establishment of cell polarity')}.
process(P that occurs_in(X)) --> [maintenance,of],cell(X),[polarity],{class_label_exact(P,'maintenance of cell polarity')}.


%% ----------------------------------------
%% cells
%% ----------------------------------------
% todo - move to it's own page? only if we want to decompose cell

% (stellate cell precursor) proliferation
cell(Cell that develops_into(MatureC)) --> cell5(MatureC),[precursor],{class_label_exact(Cell,cell),belongs(Cell,cell)}.
cell(Cell that develops_into(MatureC)) --> cell5(MatureC),[precursors],{class_label_exact(Cell,cell),belongs(Cell,cell)}.
cell(Cell that develops_into(MatureC)) --> cell5(MatureC),[precursor],[cell],{class_label_exact(Cell,cell),belongs(Cell,cell)}.

% (germarium-derived oocyte) fate determination
cell(Cell that derives_from(Source)) --> terminal(Source),['-'],[derived],cell(Cell).
