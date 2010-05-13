:- [bio(obol_biological_process_xp)].
:- [bio(obol_biological_process_xp_cell)].
:- [bio(obol_biological_process_xp_chemical)].
:- [bio(obol_biological_process_xp_cellular_component)].

:- multifile process/3,process5/3.
:- multifile quality/3.
:- multifile term_label/3.

term_label(Q) --> quality_in_bearer(Q).

% bp x quality
process(P that has_quality(Q)) --> quality(Q),process(P),{\+class_label_exact(Q,up),\+class_label_exact(Q,down)}.

%process(P that has_quality(Q)) --> process_adj(Q),process(P).
%process_adj(anon(Adj,quality)) --> [Adj],{adj(Adj,biological_process)}.

process(P that regulates(Q)) --> regulation(P),[of],quality(Q).
process(P that negatively_regulates(Q)) --> [negative],regulation(P),[of],quality(Q).
process(P that positively_regulates(Q)) --> [positive],regulation(P),[of],quality(Q).

% regulation of blood pressure by hormones
process(P that regulates(Q) and has_enabler(C)) --> regulation(P),[of],quality(Q),[by],continuant(C).

% regulation of inhibitory postsynaptic membrane potential
% regulation of blood pressure
process(P that regulates(Q)) --> regulation(P),[of],quality_in_bearer(Q).
process(P that negatively_regulates(Q)) --> [negative],regulation(P),[of],quality_in_bearer(Q).
process(P that positively_regulates(Q)) --> [positive],regulation(P),[of],quality_in_bearer(Q).

process(P that results_in_increase_in(Q)) --> elevation(P),[of],quality_in_bearer(Q).
process(P that results_in_increase_in(Q)) --> quality_in_bearer(Q),elevation(P).

process(P that results_in_decrease_in(Q)) --> reduction(P),[of],quality_in_bearer(Q).
process(P that results_in_decrease_in(Q)) --> quality_in_bearer(Q),reduction(P). % pH reduction

elevation(P) --> [elevation],{class_label_exact(P,'homeostatic process')}.
reduction(P) --> [reduction],{class_label_exact(P,'homeostatic process')}.




%establishment and/or maintenance of apical/basal cell polarity
% Polarization of a cells architecture along its apical/basal axis so that the apical and basal regions of the cell have different membrane, extracellular matrix and sub-membrane cellular components
% establishment of cell polarity : The specification and formation of anisotropic intracellular organization or cell growth patterns.
% establishment and/or maintenance of chromatin architecture : The specification, formation and maintenance of the physical structure of eukaryotic chromatin

/* ----------------------------------------
 is_a hierarchy

   E a/o M
    E
    M

  is_a GO:0009987 cellular process
   is_a GO:0007163 establishment and/or maintenance of cell polarity
    is_a GO:0030010 establishment of cell polarity
     is_a GO:0035089 establishment of apical/basal cell polarity
    is_a GO:0030011 maintenance of cell polarity
     is_a GO:0035090 maintenance of apical/basal cell polarity
    is_a GO:0035088 establishment and/or maintenance of apical/basal cell polarity
     is_a GO:0035089 establishment of apical/basal cell polarity
     is_a GO:0035090 maintenance of apical/basal cell polarity
   
 establishment of apical/basal cell polarity :
   The specification and formation of the polarity of a cell along its apical/basal axis.

 establishment and/or maintenance of apical/basal cell polarity :
   Polarization of a cells architecture along its apical/basal axis so that the apical and basal regions of the cell have different membrane, extracellular matrix and sub-membrane cellular components

 maintenance of apical/basal cell polarity :
   Retaining the established polarization of a cell along its apical/basal axis.

process(P that results_in_specification_of(Q)) --> establishment(P),[of],quality_in_bearer(Q).
process(P that results_in_maintenance_of(Q)) --> maintenance(P),[of],quality_in_bearer(Q).
process(P that results_in_specification_or_maintenance_of(Q)) --> maintenance(P),[of],quality_in_bearer(Q).

process(P that results_in_maintainenance_of(Q that inheres_in(Bearer))) --> maintenance(P),[of],continuant(Bearer),quality(Q).
process(P that results_in_specification_of(Q that inheres_in(Bearer))) --> establishment(P),[of],continuant(Bearer),quality(Q).

establishment_and_maintenance(P) --> [establishment,and,'/',or,maintenance],{class_label_exact(P,'biological_process')}.
establishment(P) --> [establishment],{class_label_exact(P,'biological_process')}.
maintenance(P) --> [maintenance],{class_label_exact(P,'biological_process')}.
   
---------------------------------------- */

% the genus is always ...CELL polarity, even though some children are eg epithelium
polarity_establishment(P) --> [establishment],{class_label_exact(P,'establishment of cell polarity')}.
polarity_maintenance(P) --> [establishment],{class_label_exact(P,'maintenance of cell polarity')}.
polarity_establishment_or_maintenance(P) --> [establishment,and,'/',or,maintenance],{class_label_exact(P,'establishment and/or maintenance of cell polarity')}.

polarity(Q) --> any_kind_of(Q,polarity).
polarity(Q) --> [polarity],{class_label_exact(Q,polarity)}.

% X of T cell polarity -- we treat all as regulation, specific type provided by X
process5(P that regulates(Q that inheres_in(C))) --> polarity_establishment(P),[of],continuant(C),polarity(Q).
process5(P that regulates(Q that inheres_in(C))) --> polarity_maintenance(P),[of],continuant(C),polarity(Q).
process5(P that regulates(Q that inheres_in(C))) --> polarity_establishment_or_maintenance(P),[of],continuant(C),polarity(Q).

% membrane depolarization
process5(P that results_in_change_to(Q that inheres_in(C))) --> continuant(C),change_in_potential(Q),{class_label_exact(P,'biological_process')}.

change_in_potential(Q) --> [depolarization],{class_label_exact(Q,'decreased action potential')}.
change_in_potential(Q) --> [polarization],{class_label_exact(Q,'increased action potential')}.
change_in_potential(Q) --> [hyperpolarization],{class_label_exact(Q,'decreased action potential')}. % TODO

% blood pressure regulation
process(P that regulates(Q)) --> quality_in_bearer(Q),regulation(P),{\+class_label_exact(Q,up),\+class_label_exact(Q,down)}.

% handled in self
%process(P that regulates(Q that inheres_in(Bearer)) and mediated_by(En)) --> regulation(P),[of],continuant(Bearer),quality(Q),[by],continuant(En).
%process(P that negatively_regulates(Q that inheres_in(Bearer)) and mediated_by(En)) --> [negative],regulation(P),[of],continuant(Bearer),quality(Q),[by],continuant(En).
%process(P that positively_regulates(Q that inheres_in(Bearer)) and mediated_by(En)) --> [positive],regulation(P),[of],continuant(Bearer),quality(Q),[by],continuant(En).

% response to hypoxia
process5(P that depends_on(C)) --> stress_response(P),[to],stress(C).
stress_response(P) --> [response],{class_label_exact(P,'response to stress')}.
stress(C) --> terminal(C),{belongs(C,'MPheno.ontology')}.
stress(C) --> terminal(C),{belongs(C,'plant_trait_ontology')}.
stress(C) --> quality(C).

% heart rate
quality_in_bearer(Q that inheres_in(Bearer)) --> anatomical_continuant(Bearer),quality(Q).
% mitochondrial calcium ion concentration
quality_in_bearer(Q that inheres_in(Bearer) and towards(C)) --> anatomy_relational_adj(Bearer),chemical(C),quality(Q).
quality_in_bearer(Q that inheres_in(Bearer) and towards(C)) --> anatomical_continuant(Bearer),chemical(C),quality(Q).
quality_in_bearer(Q that towards(C)) --> chemical(C),quality(Q).
quality_in_bearer(Q) --> quality(Q).

%% MOPs

% modification by symbiont of host structure
process5(P that regulates(Q that inheres_in(Org)) and regulated_by(RegBy)) --> [modification],[by],biological_role(RegBy),[of],biological_role(Org),quality(Q),{class_label_exact(P,'biological_process')}.

         
% modification by symbiont of host chloroplast [quality implicit]
process5(P that regulates(Q that inheres_in(C that part_of(Org))) and regulated_by(RegBy)) --> [modification],[by],biological_role(RegBy),[of],biological_role(Org),anatomical_continuant(C),{class_label_exact(Q,structure),class_label_exact(P,'biological_process')}.




%% -- QUALITIES --
term_label(Q) --> quality(Q).
quality(Q) --> quality5(Q).
%quality(Q that inheres_in(Bearer)) --> continuant(Bearer),quality(Q). LOOP!!
quality5(Q) --> terminal(Q),{belongs(Q,quality)}.


