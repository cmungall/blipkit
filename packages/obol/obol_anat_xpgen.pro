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

def(gross_anatomical(A that 'part_of'(B))) -->
        ['A'],continuant(A),[that,is,part,of,a],continuant(B),['[Obol].'].
def(gross_anatomical(A that has_quality(B))) -->
        ['Any'],continuant(A),[that,has,the,quality,of,being],quality(B),['[Obol].'].

quality(S) --> terminal(S).



