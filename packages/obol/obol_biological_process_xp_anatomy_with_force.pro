:- [bio(obol_biological_process_xp_anatomy)].

:- multifile anatomical_continuant/3.

anatomical_continuant(C) --> force(cell(C),anatomy).  % cannot use terminal in force, so we 'fake' it with cell


