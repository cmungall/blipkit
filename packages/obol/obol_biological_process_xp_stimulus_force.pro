:- [bio(obol_biological_process_xp_stimulus)].

:- multifile stimulus/3.

stimulus(C) --> force(cell(C),stimulus).  % cannot use terminal in force, so we 'fake' it with cell


