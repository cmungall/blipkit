:- [bio(obol_obo_xp)]. 

:- multifile anatomical_continuant/3,gross_anatomical/3,term_label/3,spatial/3.


term_label(X) --> anatomical_continuant(X).
term_textdef(C) --> def(gross_anatomical(C)).

anatomical_continuant(C) --> anatomical_continuant5(C).

% e.g. nucleus envelope. 
gross_anatomical(Part that part_of(Whole)) --> gross_anatomical5(Whole),gross_anatomical(Part).

% e.g. Nucleus of optic tract (note: Nucleus not in NIF_anatomy as a general class)
% e.g. Medial lemniscus of pons
gross_anatomical(Part that part_of(Whole)) --> gross_anatomical5(Part),[of],gross_anatomical(Whole).

% CA3 part of stratum radiatum ==> 'regional part of brain' THAT overlaps CA3 AND part_of 'stratum radiatum'
gross_anatomical(C that part_of(Whole) and overlaps(Section)) --> gross_anatomical5(Section),[part,of],gross_anatomical(Whole),{class_label_exact(C,'regional part of brain')}.

% e.g. regional part of hypothalamus => 'regional part of brain' THAT regional_part_of hypothalamus
%    note it would be less redundant to use 'biomaterial object' as the genus term, but for now we avoid classes
%    outside NIF_GA
gross_anatomical(Part that regional_part_of(Whole)) --> regional_part_of(Part),gross_anatomical(Whole).

% genus
regional_part_of(C) --> [regional,part,of],{class_label_exact(C,'regional part of brain')}.
regional_part_of(C) --> [regional,parts,of],{class_label_exact(C,'regional part of brain')}.

% e.g. nuclear envelope
anatomical_continuant5(Part that part_of(Whole)) --> anatomy_relational_adj(Whole),anatomical_continuant(Part).
