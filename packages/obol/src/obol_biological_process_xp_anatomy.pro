:- [bio(obol_biological_process_xp)].
:- [bio(obol_anatomy_xp)].

:- multifile process/3,process5/3,anatomical_continuant/3.
:- multifile def/3.

% see
% http://www.geneontology.org/GO.process.guidelines.shtml#dev

% (central nervous system) neuron axonogenesis
% TODO: do we need this? will be caught anyway by anat xp: "neuron part_of CNS"
% -- this was causing strange parses ---
%%%%%T process5(P that Diff and occurs_in(OuterC)) --> anatomical_continuant(OuterC),process(P that Diff).

process5(P that results_in_determination_of(C)) --> [determination],anatomical_continuant(C),{class_label_exact(P,'developmental process')}.

% cell-matrix adhesion
process5(P that results_in_connection_of(C) and results_in_connection_of(C)) --> anatomical_continuant(C),['-'],anatomical_continuant(C),adhesion(P). % TODO: QCR
process5(P that results_in_connection_of(C)) --> anatomical_continuant(C),adhesion(P).
adhesion(P) --> any_kind_of(P,'biological adhesion').
adhesion(P) --> [adhesion],{class_label_exact(P,'biological adhesion')}.

% lung induction
process5(P that induces(C)) --> anatomical_continuant(C),[induction],{class_label_exact(P,'developmental induction')}.

% notochord [cell differentiation]
% note: we could do this based on cell type, but there will be many cell types that
% are purely differentiated on the basis of gross anatomical location
process5(P that occurs_in(C)) --> gross_anatomical(C),process(P).

% nerve maturation = a developmental maturation that results in the progression of a nerve
process5(P that results_in_maturation_of(C)) --> anatomical_continuant(C),developmental_process(P).
% morphogenesis of an epithelium
process5(P that results_in_morphogenesis_of(C)) --> developmental_process(P),[of],det,anatomical_continuant(C).
% nerve development = a developmental process that results in the *complete* development of a nerve, from formation to maturation
process5(P that results_in_complete_development_of(C)) --> anatomical_continuant(C),development(P).
% CNS interneuron axonogenesis TODO generalize?
process5(P that results_in_developmental_progression_of(InnerC) and occurs_in(OuterC)) --> anatomical_continuant(OuterC),anatomical_continuant(InnerC),development(P).

% note that development is not the same as developmental process
%  - development is complete, from formation to maturation
%  - a developemntal process is a part of this
%%%%% X morphogenesis etc do not stand in a developmental_progression relation to X
development(P) --> [development],{class_label_exact(P,'developmental process')}.
developmental_process(P) --> any_kind_of(P,'developmental process'),{\+class(P,'cell differentiation')}.  % NOT T-cell differentiation
% this also catches X morphogenesis


def(process(P that results_in_developmental_progression_of(C))) -->
        {class_label_exact(P,'developmental process')},
        [process,whose,specific,outcome,is,the,progression,of,the],
        anatomical_continuant(C),
        [over,time,','],
        [from,its,formation,to,the,mature,structure,'.'].

process5(P that results_in_increase_in_mass_of(C)) --> anatomical_continuant(C),growth(P).
growth(P) --> any_kind_of(P,'growth').


% autophagic vacuole formation
process5(P that results_in_formation_of(C)) --> anatomical_continuant(C),formation(P).
process5(P that results_in_formation_of(C)) --> formation(P),[of],anatomical_continuant(C).
formation(P) --> any_kind_of(P,'anatomical structure formation').
formation(P) --> [formation],{class_label_exact(P,'anatomical structure formation')}.

% heart process
process5(P that has_agent(C)) --> anatomical_continuant(C),[process],{class_label_exact(P,'biological_process')}.

% coating and uncoating TODO: requires generic coat term - CARO?

% wing disc pattern formation
% -- (will not parse currently, as fly_anat uses only related)
process5(P that occurs_in(C)) --> anatomical_continuant(C),pattern_specification(P).
pattern_specification(P) --> any_kind_of(P,'pattern specification process').
pattern_specification(P) --> [pattern],[formation],{class_label_exact(P,'pattern specification process')}.
def(process(P that occurs_in(C))) -->
        {subclassT(P,'pattern specification process')},
        ['A'],pattern_specification(P),['giving rise to the pattern of cell differentiation in a'],anatomical_continuant(C).

% somite specification: The process by which individual somites establish identity during embryogenesis
process5(P that results_in_specification_of(C)) --> anatomical_continuant(C),specification(P). % TODO: new relation??
specification(P) --> [specification],{class_label_exact(P,'pattern specification process')}.
% see also cell fate specification in bp_xp_cell

% elongation of artista lateral; spindle elongation
process5(P that results_in_increased_length_of(C)) --> anatomical_continuant(C),elongation(P).
process5(P that results_in_increased_length_of(C)) --> elongation(P),[of],anatomical_continuant(C).
elongation(P) --> [elongation],{class_label_exact(P,'biological_process')}.


% nurse cell nucleus anchoring
process5(P that results_in_connection_of(C1) and results_in_connection_of(C2)) --> anatomical_continuant(C1),anatomical_continuant(C2),attachment(P).
attachment(P) --> [attachment],{class_label_exact(P,biological_process)}.
attachment(P) --> [anchoring],{class_label_exact(P,biological_process)}.
def(process(P that results_in_connection_of(C1) and results_in_connection_of(C2))) -->
        ['A'],terminal(P),['by which a'],terminal(C1),['and a'],terminal(C2),['become attached'].

% neural tube closure
process5(P that results_in_closure_of(C)) --> anatomical_continuant(C),[closure],{class_label_exact(P,'developmental process')}.

% oocyte maturation
process5(P that results_in_developmental_progression_of(C)) --> anatomical_continuant(C),maturation(P).
maturation(P) --> any_kind_of(P,'developmental maturation').
maturation(P) --> [maturation],{class_label_exact(P,'developmental maturation')}.

% larval pigmentation
process(P that occurs_in(C)) --> anatomy_relational_adj(C),process(P). % defined in obol_anatomy_xp

% neuroblast delamination
% TODO

% neuron remodeling TODO: state explicitly what has changed
%  The developmentally regulated remodeling of neuronal projections such as pruning to eliminate the extra dendrites and axons projections
%  set up in early stages of nervous system development
process5(P that results_in_remodeling_of(C)) --> anatomical_continuant(C),remodeling(P).
remodeling(P) --> [remodeling],{class_label_exact(P,'developmental process')}.

% blood vessel remodeling : The reorganization or renovation of existing blood vessels
def(process5(P that results_in_remodeling_of(C))) -->
        ['reorganization or renovation of'],
        anatomical_continuant(C),
        ['; A kind of'],
        remodeling(P).

% neuron recognition
process5(P that has_agent(C)) --> anatomical_continuant(C),cell_recognition(P).
def(process(P that has_agent(C))) -->
        any_kind_of(P,'cell recognition'),
        ['A',P,'by which a',C,'recognizes its surroundings'].

process5(P that has_agent(C1) and has_agent(C2)) --> anatomical_continuant(C1),['-'],anatomical_continuant(C2),cell_communication(P). % TODO: QCR
cell_communication(P) --> any_kind_of(P,'cell communication').

% cell--matrix recognition
process5(P that has_agent(C1) and has_agent(C2)) --> anatomical_continuant(C1),['-'],anatomical_continuant(C2),cell_recognition(P). % TODO: QCR
def(process(P that has_agent(C1) and has_agent(C2))) -->
        any_kind_of(P,'cell recognition'),
        ['Cell recognition that involves the interaction of a',C1,'with a',C2].

cell_recognition(P) --> [recognition],{class_label_exact(P,'cell recognition')}.

process5(P that results_in_separation_of_bundle_of(C)) --> anatomical_continuant(C),separation(P).
process5(P that results_in_separation_of_bundle_of(C)) --> separation(P),[of],anatomical_continuant(C).
separation(P) --> [defasciculation],{class_label_exact(P,'biological process')}.

% (imaginal disc-derived genitalia) morphogenesis
anatomical_continuant(MatureC that derives_from(ImmatureC)) --> terminal(MatureC),['-'],[derived],anatomical_continuant(ImmatureC).


