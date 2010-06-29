:- [bio(obol_cellular_component_xp)]. 


:- multifile molecular_function/3,molecular_function5/3.
:- multifile term_label/3.

term_label(P) --> molecular_function(P).

% structural constituent of chromatin : also works for gross anatomy: eg bone
molecular_function(F that inheres_in(C)) --> [structural],[constituent],[of],continuant(C),{class_label_exact(F,'structural molecule activity')}.

% watch out for circularity!
% microtubule motor activity
molecular_function(F that inheres_in(C)) --> cellular_component5(C),molecular_function(F),{ \+ class_label_exact(F,binding)}.

molecular_function(F that executes(P that results_in_binding_of(X))) --> continuant(X),binding(F),{class_label_exact(P,'biological_process')}.

molecular_function(F that inheres_in(C)) --> [transmembrane],molecular_function(F),{class_label_exact(C,'integral to membrane')}.
molecular_function(F that inheres_in(C)) --> [ionotropic],molecular_function(F),{class_label_exact(C,'ion channel')}.

% TODO
% this is in mf_xp_chem too......
binding(P) --> [binding],{class_label_exact(P,binding)}. % used in: binding of sperm to zona pellucida


