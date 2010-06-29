:- [bio(obol_biological_process_xp_anatomy)].
:- [bio(obol_cellular_component_xp)].

:- multifile process/3,process5/3,anatomical_continuant/3.

% plastid
process5(P that results_in_division_of(CC)) --> cellular_component(CC),[fission],{class_label_exact(P,'cellular process')}.
% mitochondrial fission; cytoplasmic transport, nurse cell to oocyte
process5(P that results_in_division_of(CC)) --> [Adj],[fission],{relational_adj_ra(Adj,Noun,cellular_component),class(CC,Noun),class_label_exact(P,'cellular process')}.
def(process(_ that results_in_division_of(CC))) -->
        ['The creation of two or more'],cellular_component(CC),['s by division of one'],cellular_component(CC).

% organelle fusion
% myoblast fusion : The fusion of myoblasts to form myotubes
process5(P that results_in_fusion_of(CC)) --> cellular_component(CC),[fusion],{class_label_exact(P,'cellular process')}.
% mitochondrial fusion : Merging of two or more mitochondria within a cell to form a single compartment
process5(P that results_in_fusion_of(CC)) --> [Adj],[fusion],{relational_adj_ra(Adj,Noun,cellular_component),class(CC,Noun),class_label_exact(P,'cellular process')}.


def(process(_ that results_in_fusion_of(CC))) -->
    ['Merging of of two or more'],
    cellular_component(CC),
    ['s within a cell to form a single component'].

% synaptic vesicle to endosome fusion : Fusion of a synaptic vesicle with the membrane of an endosome
process5(P that results_in_fusion_of(C1) and results_in_fusion_of(C2)) -->
        cellular_component(C1),[to],cellular_component(C2),[fusion],{class_label_exact(P,'cellular process')}.
% synaptic vesicle fusion to presynaptic membrane
process5(P that results_in_fusion_of(C1) and results_in_fusion_of(C2)) -->
        cellular_component(C1),[fusion],[to],cellular_component(C2),[fusion],{class_label_exact(P,'cellular process')}.
% vesicle fusion with nuclear membrane
process5(P that results_in_fusion_of(CC) and results_in_fusion_of(With)) --> cellular_component(CC),[fusion],[with],cellular_component(With),{class_label_exact(P,'cellular process')}.
% viral envelope fusion with host membrane - TODO

def(process(_ that results_in_fusion_of(C1) and results_in_fusion_of(C2))) -->
        ['Merging of a'],
        cellular_component(C1),
        ['with a'],
        cellular_component(C2),
        ['to form a single component'].


% microtubule based process
process5(P that has_participant(CC)) --> cellular_component(CC),['-',based],process(P).

% see http://sourceforge.net/tracker/index.php?func=detail&aid=1733770&group_id=36855&atid=440764
% - org and biogenesis
%  - assembly
%  - disassembly
% A po O+B
% A po B+A
% C O+B includes disassembly of parts of C
% nuclear envelope disassembly : The controlled breakdown of the nuclear envelope in the context of a normal process
process5(P that occurs_in(Cell) and results_in_organization_of(CC)) --> cell(Cell),cellular_component(CC),cellular_component_organization(P).
process5(P that results_in_organization_of(CC)) --> cellular_component(CC),cellular_component_organization(P).
%TODOcellular_component_organization(P) --> [biogenesis],{class_label_exact(P,'cellular component organization')}. 
cellular_component_organization(P) --> [organization],{class_label_exact(P,'cellular component organization')}. 
cellular_component_organization(P) --> [organization,and,biogeneisis],{class_label_exact(P,'cellular component organization')}. 
%cellular_component_organization(P) --> [biogenesis],{class_label_exact(P,'cellular component organization')}. 

process5(P that results_in_formation_of(C)) --> cellular_component(C),complex_assembly(P).
process5(P that results_in_formation_of(C)) --> complex_assembly(P),[of],cellular_component(C).


def(process5(_P that results_in_formation_of(C))) -->
        ['The aggregation and bonding together of molecules to form a'],
        cellular_component(C).

process5(P that results_in_breakdown_of(C)) --> cellular_component(C),complex_disassembly(P).
process5(P that results_in_breakdown_of(C)) --> complex_disassembly(P),[of],cellular_component(C).

complex_disassembly(P) --> any_kind_of(P,'cellular component disassembly').
complex_disassembly(P) --> [disassembly],{class_label_exact(P,'cellular component disassembly')}.

process5(P that results_in_creation_of(C)) --> cellular_component(C),complex_assembly(P).

% spore wall assembly
% autophagic vacuole formation [DEF: "The formation of a double membrane-bound structure, the autophagosome, that occurs when a specialized membrane sac, called the isolation membrane, starts to enclose a portion of the cytoplasm."]
complex_assembly(P) --> any_kind_of(P,'cellular component assembly').
complex_assembly(P) --> [assembly],{class_label_exact(P,'cellular component assembly')}.
%complex_assembly(P) --> [formation],{class_label_exact(P,'cellular component assembly')}. % TODO: check for overlap with anatomical structure formation. autophagic vacuole does NOT follow pattern
% todo: are these significantly different? YES
%complex_assembly(P) --> [assembly,and,maintenance],{class_label_exact(P,'cellular component assembly')}.
%complex_assembly(P) --> [biogenesis],{class_label_exact(P,'cellular component assembly')}. % TODO: check
%complex_assembly(P) --> [biogenesis,and,assembly],{class_label_exact(P,'cellular component assembly')}. % TODO: check
complex_assembly(P) --> [complex,assembly],{class_label_exact(P,'cellular component assembly')}.

def(process(_ that results_in_creation_of(C))) -->
        ['The aggregation and bonding together of a set of macromolecules to form a'],
        cellular_component(C).

% as parts..?
def(process(_ that results_in_creation_of(C) and results_in_maintenance_of(C))) -->
        ['The aggregation and bonding together of a set of macromolecules to form and maintain a'],
        cellular_component(C).

% vesicle docking : The initial attachment of a transport vesicle membrane to the target membrane, mediated by proteins protruding from the membrane of the vesicle and the target membrane. Docking requires only that the two membranes come close enough for these proteins to interact and adhere

process5(P that results_in_connection_of(CC) and results_in_connection_to(TargetMembrane)) -->
        cellular_component(CC),[docking],{class_label_exact(P,'membrane docking'),class_label_exact(TargetMembrane,membrane)}.
% do we need to capture *how* they are connected? adherence?

% protein to membrane docking
%  The initial attachment of a protein to a target membrane, mediated by a proteins protruding from the target membrane.
%  Docking requires only that the proteins come close enough to interact and adhere
process5(P that results_in_connection_of(CC) and results_in_connection_to(Target)) -->
        cellular_component(CC),[to],cellular_component(Target),[docking],{class_label_exact(P,'membrane docking')}.

def(process5(P that results_in_connection_of(CC) and results_in_connection_to(Target))) -->
        ['initial attachment of a '],
        cellular_component(CC),
        ['to a target'],
        cellular_component(Target),
        ['mediated by a proteins protruding from the target membrane.'],
        process(P),{class_label_exact(P,'membrane docking')},
        ['requires only that the proteins come close enough to interact and adhere'].

% chromatin silencing at rDNA
process(P that unfolds_around(CC)) --> process5(P),[at],cellular_component(CC).


% synaptic vesicle budding
% process5(P ) --> cellular_component(CC) 
