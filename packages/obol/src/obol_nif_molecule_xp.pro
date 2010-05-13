:- [bio(obol_obo_xp)]. 

:- multifile anatomical_continuant/3,gross_anatomical/3,term_label/3,spatial/3.


term_label(X) --> molecule(X).
term_textdef(C) --> def(molecule(C)).

% extracellular matrix protein
protein(Pr that part_of(C)) --> cellular_component(C),protein(Pr).

% cytoskeletal protein
protein(Pr that part_of(C)) --> anatomy_relational_adj(C),protein(Pr).

% cell adhesion molecule; transporter protein
protein(Pr that has_function(F)) --> molecular_function(F),protein(Pr).
protein(Pr that has_function(F)) --> biological_process(F),protein(Pr).

% dopamine receptor
protein(Pr that binds_to(M)) --> molecule(M),protein(Pr).

