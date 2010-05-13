%:- [bio(obol_phenotype_xp)].
%:- [bio(obol_phenotype_attribute_value)].
:- [bio(obol_obo_xp)].
:- [bio(obol_quality_xp)].
:- multifile term_label/3.
:- multifile phenotype/3.
:- multifile anatomical_continuant/3.
:- multifile quality/3.

term_label(P) --> phenotype(P).

rel_quality(Q that towards(C)) --> anatomical_continuant(C),[number],{class_label_exact(Q,'has number of')}.
rel_quality(Q that towards(C)) --> anatomical_continuant(C),[number,increased],{class_label_exact(Q,'has extra parts of type')}.
rel_quality(Q that towards(C)) --> anatomical_continuant(C),[number,decreased],{class_label_exact(Q,'has fewer parts of type')}.
rel_quality(Q that towards(C)) --> quality(Q),anatomical_continuant(C), {\+ class_label_exact(Q,'has number of'), \+ class_label_exact(Q,'has extra parts of type'), \+ class_label_exact(Q,'has fewer parts of type')}.
phenotype(Q that inheres_in(C) and towards(C2)) --> rel_quality(Q that towards(C2)),[in],anatomical_continuant(C).
phenotype(Q that inheres_in(C)) --> anatomical_continuant(C),quality(Q).

anatomical_continuant(C that part_of(W)) --> gross_anatomical5(C),continuant(W).
anatomical_continuant(W that has_part(P)) --> gross_anatomical5(W),[with],continuant(P).
