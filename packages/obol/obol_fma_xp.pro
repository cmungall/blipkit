:- [bio(obol_obo_xp)].

:- multifile term_label/3,term_textdef/3.
:- multifile anatomical_continuant/3,anatomical_continuant5/3.

% remember to de-capitalize!

term_label(P) --> anatomical_continuant(P).
term_textdef(C) --> def(anatomical_continuant(C)).

def(anatomical_continuant(Part that part_of(Whole))) --> ['A'],terminal(Part),['that is part of a'],terminal(Whole).

anatomical_continuant(Part that part_of(Whole)) --> anatomical_continuant5(Whole),anatomical_continuant(Part).

% eg blood in aorta
anatomical_continuant(Part that part_of(Whole)) --> anatomical_continuant5(Part),[in],anatomical_continuant(Whole).

% eg trabecular bone of distal metaphysis of proximal phalanx of right thumb
anatomical_continuant(Part that part_of(Whole)) --> anatomical_continuant5(Part),[of],anatomical_continuant(Whole).

% Submucosa of zone of stomach - we want to use Region of X as genus
anatomical_continuant(Part that part_of(Whole)) --> anatomical_continuant5(X),[of],anatomical_continuant(Whole),{genus(Part,'FMA:86103'),differentium(Part,part_of,X)}.



% eg Dorsal branch of right subcostal artery
% note: branch of X is_a X
anatomical_continuant(Part that branch_of(Whole)) --> [branch,of],anatomical_continuant(Whole),{class_label_exact(Part,'anatomical structure')}.
%anatomical_continuant(Part that has_coordinate(Q) and branch_of(Whole)) --> laterality(Q),[branch,of],anatomical_continuant(Whole),{class_label_exact(Part,'anatomical structure')}.

% Transverse process of tenth thoracic vertebra
anatomical_continuant(Part that branch_of(Whole)) --> [process],[of],anatomical_continuant(Whole),{class_label_exact(Part,'process of organ')}.
%anatomical_continuant(Part that has_coordinate(Q) and branch_of(Whole)) --> laterality(Q),[process],[of],anatomical_continuant(Whole),{class_label_exact(Part,'process of organ')}.

% Region of bronchus
anatomical_continuant(Part that part_of(Whole)) --> [region],[of],anatomical_continuant(Whole),{class_label_exact(Part,'region of organ component'),subclassT(Whole,Part)}. % FMA:86103 ! Region of organ component




anatomical_continuant(CC) --> anatomical_continuant5(CC).



% Lateral surface of body of eighth thoracic vertebra
%anatomical_continuant5(Part that has_coordinate(Q)) --> laterality(Q),anatomical_continuant5(Part).
%laterality(Q) --> any_kind_of(Q,'primary anatomical coordinate').



% Diaphragmatic...

anatomical_continuant5(C) --> [muscle],{class_label_exact(C,'muscle organ')}.
anatomical_continuant5(CC) --> any_kind_of(CC,'physical anatomical entity').
anatomical_continuant5(CC) --> any_kind_of(CC,'material anatomical entity').



