:- [bio(obol_obo_xp)]. 

:- multifile anatomical_continuant/3,cell5/3,term_label/3,term_textdef/3.

term_label(CC) --> cell(CC).

% nucleate erythrocyte
cell5(C that has_part(N)) --> [nucleate],cell(C),{class_label_exact(N,nucleus)}.
cell5(C that lacks_part(N)) --> [anucleate],cell(C),{class_label_exact(N,nucleus)}.
cell5(C that lacks_part(N)) --> [enucleate],cell(C),{class_label_exact(N,nucleus)}.

% binucleate cell
%cell5(C that has_part(N,exactly(2))) --> [binucleate],cell(C),{class_label_exact(N,nucleus)}.
