:- [bio(obol_obo_xp)].

:- multifile molecular_function/3,molecular_function5/3.
:- multifile term_label/3.

term_label(P) --> molecular_function(P).

% methotrexate binding
molecular_function(P that results_in_binding_of(X)) --> continuant(X),binding(P).
binding(P) --> [binding],{class_label_exact(P,binding)}. % used in: binding of sperm to zona pellucida
