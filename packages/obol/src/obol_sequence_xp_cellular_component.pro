:- [bio(obol_cellular_component_xp)].

:- multifile process/3, process5/3.
:- multifile term_label/3.
:- multifile biosequence/3.

biosequence(X) --> cellular_component(C),[sequence].
