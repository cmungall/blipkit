:- [bio(obol_obo_xp)]. 
:- [bio(obol_anatomy_xp)]. % gross_anatomical


:- multifile anatomical_continuant/3,cell/3,cell5/3,term_label/3,term_textdef/3.

% (we must be careful here to avoid loops)
% glucocorticoid secreting cell; collagen secreting cell
cell(C that has_output(Out)) --> secreted_protein(Out),secreting_cell(C).
secreting_cell(C) --> [secreting],[cell],{class_label_exact(C,'secretory cell')}.
secreting_cell(C) --> any_kind_of(C,'secretory cell').

% bombesin stimulating hormone secreting cell
secreted_protein(H that stimulates(P)) --> force(sp5(P),protein),[stimulating],hormone(H).
secreted_protein(P) --> force(sp5(P),protein).
hormone(H) --> any_kind_of(H,hormone).
sp5(P) --> terminal(P),{\+belongs(P,cell)}. % -ve clause necessary for force (otherwise always succeeds with whole cell term)
anatomical_continuant(C) --> secreted_protein(C). % needed so we can generate as well as parse




