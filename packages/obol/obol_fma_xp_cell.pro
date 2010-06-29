:- [bio(obol_obo_xp)].

:- multifile term_label/3,term_textdef/3.
:- multifile anatomical_continuant/3,anatomical_continuant5/3.

% remember to de-capitalize!

term_label(P) --> anatomical_continuant(P).
term_textdef(C) --> def(anatomical_continuant(C)).

% eg Pleura of diaphragmatic surface of middle lobe
anatomical_continuant(Part that part_of(Whole)) --> anatomical_continuant5(Whole),fmacell(Part).

% eg trabecular bone of distal metaphysis of proximal phalanx of right thumb
anatomical_continuant(Part that part_of(Whole)) --> fmacell(Part),[of],anatomical_continuant(Whole).

fmacell(C) --> terminal(C),{subclassRT(C,'FMA:68646')}.




