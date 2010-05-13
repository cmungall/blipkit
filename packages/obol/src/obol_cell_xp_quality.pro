:- [bio(obol_obo_xp)]. 
:- [bio(obol_quality_xp)]. 

:- multifile anatomical_continuant/3,cell5/3,term_label/3,term_textdef/3.

% haploid cell
% also false +ves like "red blood cell"
cell5(C that has_quality(Q)) --> quality(Q),cell(C).



