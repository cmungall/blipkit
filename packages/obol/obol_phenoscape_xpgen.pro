:- [bio(obol_obo_xp)].
:- [bio(obol_anatomy_xp)].
:- [bio(obol_quality_xp)].

:- multifile term_label/3.
:- multifile bearer/3.
:- multifile gross_anatomical5/3.
:- multifile phenotype/3.
:- multifile def/3.
:- multifile term_textdef/3.

term_label(P) --> phenotype(P).
term_label(P) --> anat_tax(P).

term_textdef(P) --> def(phenotype(P)).

anat_tax(A that 'PHENOSCAPE:in_taxon'(T)) --> taxon(T),anatomical_continuant(A).

anatomical_continuant(A) --> terminal(A),{id_idspace(A,'TAO')}.
gross_anatomical5(T) --> taxon(T).
taxon(T) --> terminal(T),{id_idspace(T,'TTO')}.

