:- [bio(obol_molecular_function_xp_receptor)].

:- multifile molecular_entity/3.

molecular_entity(C) --> force(cell(C),molecular_entity).  % cannot use terminal in force, so we 'fake' it with cell


