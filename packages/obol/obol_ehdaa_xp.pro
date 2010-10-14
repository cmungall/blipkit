:- [bio(obol_obo_xp)].

:- multifile term_label/3,term_textdef/3.
:- multifile anatomical_continuant/3,anatomical_continuant5/3.

term_label(P) --> anatomical_continuant(P).
term_textdef(C) --> def(anatomical_continuant(C)).

anatomical_continuant(Part that part_of(Whole)) --> anatomical_continuant5(Whole),anatomical_continuant(Part).

% eg blood in aorta
anatomical_continuant(Part that part_of(Whole)) --> anatomical_continuant5(Part),[in],anatomical_continuant(Whole).

% eg future brain
anatomical_continuant(G that develops_into(X)) --> [future],anatomical_continuant(X),{class_label_exact(G,'embryonic anatomical structure')}.

% eg trabecular bone of distal metaphysis of proximal phalanx of right thumb
anatomical_continuant(Part that part_of(Whole)) --> anatomical_continuant5(Part),[of],anatomical_continuant(Whole).

% 1st arch mandibular mesenchyme from head mesoderm
anatomical_continuant(G that part_of(Part) and develops_from(Whole)) -->
        anatomical_continuant5(Part),[from],anatomical_continuant(Whole),{class_label_exact(G,'embryonic anatomical structure')}.

anatomical_continuant(CC) --> anatomical_continuant5(CC).
anatomical_continuant5(CC) --> gross_anatomical5(CC).


% Lateral surface of body of eighth thoracic vertebra
%anatomical_continuant5(Part that has_coordinate(Q)) --> laterality(Q),anatomical_continuant5(Part).
%laterality(Q) --> any_kind_of(Q,'primary anatomical coordinate').



% Diaphragmatic...



