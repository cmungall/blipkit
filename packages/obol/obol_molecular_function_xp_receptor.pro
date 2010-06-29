%:- [bio(obol_biological_process_xp)].
:- [bio(obol_obo_xp)].

term_label(P) --> molecular_function(P).
term_label(P) --> cellular_component(P).

:- multifile process/3,process5/3,molecular_entity/3.
:- multifile def/3.

molecular_function(F that combines_with(E)) --> molecular_entity(E),[receptor],[activity],{class_label_exact(F,'molecular_function')}.
molecular_function(F that interacts_selectively_with(E)) --> molecular_entity(E),[receptor],[binding],{class_label_exact(F,'molecular_function')}.
molecular_function(CC that binds(E)) --> molecular_entity(E),[receptor],[complex],{class_label_exact(CC,'receptor complex')}.
