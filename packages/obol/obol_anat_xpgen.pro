:- [bio(obol_obo_xp)].

:- multifile term_label/3.
:- multifile gross_anatomical/3.
:- multifile def/3.
:- multifile term_textdef/3.

term_label(P) --> gross_anatomical(P).

term_textdef(P) --> def(gross_anatomical(P)).

gross_anatomical(A that part_of(B)) -->
	continuant(A),
        [of],
        continuant(B).

gross_anatomical(A that part_of(B)) -->
	continuant(B),
        continuant(A).


