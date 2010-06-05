/* -*- Mode: Prolog -*- */


:- module(ontol_writer_owl2,[]).

:- use_module(library('thea2/owl2_io')).
:- use_module(library('thea2/owl2_to_prolog_dlp')).

io:redirect_stdout(owl2(_)).
io:write_all(owl2_prolog,F,_):-
        save_axioms(F,prolog).
io:write_all(owl2(Fmt),F,_):-
        (   F=null(_)
        ->  F2=_
        ;   F2=F),
        save_axioms(F2,Fmt).
