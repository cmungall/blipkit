:- module(obolog_writer_latex, [
                                  ]).
:- use_module(bio(serval)).
:- use_module(bio(obolog_db)).
:- user:consult(obolog_writer_latex_sfuncs).

:- multifile io:format_writer/2.

io:format_writer(obolog_latex,obolog_writer_latex).
io:format_writer(obolog_latex_section,obolog_writer_latex).
io:format_writer(obolog_latex_section(_),obolog_writer_latex).
io:format_writer(obolog_latex_lp,obolog_writer_latex).

io:redirect_stdout(obolog_latex).

io:write_all(obolog_latex,_,_):-
        write_sterm([],xml([]),relation_document).
io:write_all(obolog_latex_section,_,_):-
        write_sterm([],xml([]),relation_sections_combined).
io:write_all(obolog_latex_section(Title),_,_):-
        write_sterm([],xml([]),relation_sections_combined(Title)).
% literate-programming style
io:write_all(obolog_latex_lp,_,_):-
        write_sterm([],xml([]),all_formulae).
