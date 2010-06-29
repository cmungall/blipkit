:- [bio(obol_biological_process_xp_protein)].

:- multifile process/3,process5/3,anatomical_continuant/3,cell/3,protein/3.

protein(C) --> force(cell(C),protein).  % cannot use terminal in force, so we 'fake' it with cell


