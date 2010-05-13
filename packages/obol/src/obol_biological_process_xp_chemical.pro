:- [bio(obol_biological_process_xp)].

:- multifile process/3,process5/3,anatomical_continuant/3.

process(P that has_input(CE)) --> process5(P),[from],chemical(CE).

% aerobic respiration, using carbon monoxide as electron donor
process(P that has_enabler(CE that has_role(Role))) --> process5(P),[','],[using],chemical(CE),[as],terminal(Role).

% lactose catabolic process, using glucoside 3-dehydrogenase
% magnetoreception, using chemical stimulus
process(P that has_enabler(CE)) --> process5(P),[','],[using],chemical(CE). % see also: MF

% peptide cross-linking via 2-imino-methionine 5-imidazolinone glycine
process(P that has_enabler(CE)) --> process5(P),[via],chemical(CE).

% conversion of seryl-tRNAsec to selenocys-tRNAsec
process5(P that results_in_transformation_from(A) and results_in_transformation_to(B)) --> [conversion],[of],chemical(A),[to],chemical(B),{class_label_exact(P,'metabolic process')}.

% anion homeostasis; 
process5(P that regulates_levels_of(C)) --> chemical(C),chemical_homeostasis(P).
chemical_homeostasis(P) --> [homeostasis],{class_label_exact(P,'chemical homeostasis')}.
chemical_homeostasis(P) --> any_kind_of(P,'chemical homeostasis').

% cell glucose homeostasis
process5(P that regulates_levels_of(C) and occurs_in(Cell)) --> cell(Cell),chemical(C),cell_homeostasis(P).
cell_homeostasis(P) --> [homeostasis],{class_label_exact(P,'cell homeostasis')}.
cell_homeostasis(P) --> any_kind_of(P,'cell homeostasis').

% chemotaxis to cAMP
process5(P that response_to(C)) --> [chemotaxis],[to],chemical(C),{class_label_exact(P,chemotaxis)}.

% sequestering of calcium ion
process5(P that acts_on(C)) --> [sequestering],[of],chemical(C),{class_label_exact(P,'maintenance of location')}.

% response to pheromone; 
process5(P that depends_on(C)) --> chemical_response(P),[to],chemical(C).
process5(P that depends_on(C)) --> chemical_response(P),[to],chemical(C),[stimulus].
process5(P that depends_on(C)) --> chemical(C),chemical_response(P).
chemical_response(P) --> [response],{class_label_exact(P,'response to chemical stimulus')}.

% X metabolism; 
process5(P that has_participant(C)) --> chemical(C),chemical_metabolism(P).
chemical_metabolism(P) --> [metabolism],{class_label_exact(P,'metabolic process')}.
chemical_metabolism(P) --> [metabolic],[process],{class_label_exact(P,'metabolic process')}.

% regulation of systemic arterial blood pressure by epinephrine: TODO - should we break down the regulation part?
process(P that mediated_by(C)) --> process5(P),[by],chemical(C).

% X catabolism; 
process5(P that results_in_breakdown_of(C)) --> chemical(C),chemical_catabolism(P).
% tyrosine catabolic process to fumarate
process5(P that results_in_breakdown_of(C) and results_in_creation_of(To)) --> chemical(C),chemical_catabolism(P),[to],chemical(To).
% glucose catabolic process to lactate and acetate
%  The anaerobic chemical reactions and pathways resulting in the breakdown of glucose to lactate and acetate, yielding energy in the form of ATP.
process5(P that results_in_breakdown_of(C) and results_in_creation_of(To1) and results_in_creation_of(To2)) --> chemical(C),chemical_catabolism(P),[to],chemical(To1),[and],chemical(To2).

% arginine catabolic process to alanine via ornithine
process5(P that results_in_breakdown_of(C) and results_in_creation_of(To) and has_intermediate(Via)) --> chemical(C),chemical_catabolism(P),[to],chemical(To),[via],chemical(Via).


chemical_catabolism(P) --> [catabolism],{class_label_exact(P,'catabolic process')}.
chemical_catabolism(P) --> any_kind_of(P,'catabolic process').

% nitrate assimilation; TODO: utilization - http://sourceforge.net/tracker/index.php?func=detail&aid=1738843&group_id=36855&atid=440764
process5(P that results_in_update_of(C)) --> chemical(C),[assimilation],{class_label_exact(P,'metabolic process')}.

% glutamate deamidation - TODO; check
process5(P that results_in_breakdown_of(C) and results_in_removal_of(Amide)) --> chemical(C),[deamidation],{class_label_exact(Amide,amide),class_label_exact(P,'catabolic process')}.

% X phosphorylation
% actin phosphorylation [DEF: "The transfer of one or more phosphate groups to an actin molecule."]
%process5(P that results_in_addition_to(C) and results_in_addition_of(PG)) --> continuant(C),phosphorylation(P),{class_label_exact(PG,'phosphate group')}.
%phosphorylation(P) --> any_kind_of(P,'phosphorylation').

% X dephosphorylation
%process5(P that results_in_removal_from(C) and results_in_removal_of(PG)) --> continuant(C),dephosphorylation(P),{class_label_exact(PG,'phosphate group')}.
%dephosphorylation(P) --> any_kind_of(P,'dephosphorylation').

% X methylation
% todo: abstract this to *ylation pattern?
%process5(P that results_in_addition_to(C) and results_in_addition_of(PG)) --> continuant(C),methylation(P),{class_label_exact(PG,'methyl group')}.
%process5(P that results_in_removal_from(C) and results_in_removal_of(PG)) --> continuant(C),demethylation(P),{class_label_exact(PG,'methyl group')}.
process5(P that results_in_addition_of(G) and results_in_addition_to(C)) -->
        continuant(C),[Reaction],{reaction_element(Reaction,_,+,GN),class_label_exact(G,GN),reaction_class_label(P,Reaction)}.
process5(P that results_in_addition_of(G) and results_in_addition_to(C)) -->
        continuant(C),['-'],[Reaction],{reaction_element(Reaction,_,+,GN),class_label_exact(G,GN),reaction_class_label(P,Reaction)}.
process5(P that results_in_removal_of(G) and results_in_removal_from(C)) -->
        continuant(C),[Reaction],{reaction_element(Reaction,_,-,GN),class_label_exact(G,GN),reaction_class_label(P,Reaction)}.
process5(P that results_in_removal_of(G) and results_in_removal_from(C)) -->
        continuant(C),['-'],[Reaction],{reaction_element(Reaction,_,-,GN),class_label_exact(G,GN),reaction_class_label(P,Reaction)}.
% note: some redundancy here - we do not need to make the genus phosphorylation??

% always map up to MP - the more specific process would be redundant
reaction_class_label(P,_L):- class_label_exact(P,'metabolic process').

%methylation(P) --> any_kind_of(P,'methylation').
%demethylation(P) --> any_kind_of(P,'demethylation').
%demethylation(P) --> [demethylation],{class_label_exact(P,'demethylation')}.

% TODO: check!
reaction_element(oxidation,oxidase,-,'electron').
reaction_element(reduction,reductase,+,'electron').

reaction_element(methylation,methylase,+,'methyl group').
reaction_element(demethylation,demethylase,-,'methyl group').

reaction_element(acetylation,acetylase,+,'acetyl group').
reaction_element(deacetylation,deacetylase,-,'acetyl group').

reaction_element(phosphorylation,phosphorylase,+,'phosphate group').
reaction_element(dephosphorylation,dephosphorylase,-,'phosphate group').

reaction_element(dephosphorylation,phosphotase,-,'phosphate group'). % diff from dephosphorylase??

%reaction_element(decarboxylation,decarboxylase,-,'carbon dioxide').
reaction_element(decarboxylation,decarboxylase,-,'carboxyl group').
reaction_element(carboxylation,carboxylase,+,'carboxyl group').


