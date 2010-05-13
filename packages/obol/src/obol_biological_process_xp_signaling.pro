:- [bio(obol_biological_process_xp)].

:- multifile process/3,process5/3,term_label/3.

process(Path that starts_with(Binding)) --> protein(R),[signaling,pathway],{differentium(Binding,'OBO_REL:results_in_binding_of',R),class_label_exact(Path,'signal transduction')}.


