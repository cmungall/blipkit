:- [bio(obol_obo_xp)]. 

:- multifile anatomical_continuant/3,anatomical_continuant5/3,gross_anatomical/3,term_label/3.

term_label(X) --> anatomical_continuant(X).
term_textdef(C) --> def(gross_anatomical(C)).

anatomical_continuant(C) --> anatomical_continuant5(C).


% genus
regional_part_of(C) --> [regional,part,of],{class_label_exact(C,'regional part of cell')}.
regional_part_of(C) --> [regional,parts,of],{class_label_exact(C,'regional part of cell')}.

% e.g. nuclear envelope
anatomical_continuant5(Part that part_of(Whole)) --> anatomy_relational_adj(Whole),anatomical_continuant(Part).

% e.g. nucleus envelope. 
anatomical_continuant(Part that part_of(Whole)) --> anatomical_continuant5(Whole),anatomical_continuant(Part).

% e.g. envelope of nucleus
anatomical_continuant(Part that part_of(Whole)) --> anatomical_continuant5(Part),[or],anatomical_continuant(Whole).

% e.g. regional part of hypothalamus => 'regional part of brain' THAT regional_part_of hypothalamus
%    note it would be less redundant to use 'biomaterial object' as the genus term, but for now we avoid classes
%    outside NIF_GA
gross_anatomical(Part that regional_part_of(Whole)) --> regional_part_of(Part),gross_anatomical(Whole).

% everything is an AC
anatomical_continuant5(X) --> terminal(X).
