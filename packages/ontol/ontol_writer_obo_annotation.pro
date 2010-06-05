/* -*- Mode: Prolog -*- */

DONT USE THIS

use curation_writer_obo

:- module(ontol_writer_obo_annotation, []).
:- multifile ontol_writer:write_class/2.

:- use_module(bio(ontol_writer)).
:- use_module(bio(ontol_writer_obo)).

io:redirect_stdout(obo_annotation).
io:write_all(obo_annotation):- io:write_all(obo).
:- mode write_class(+,+) is det.
ontol_writer:write_class(obo_annotation,ID):- write_class(obo,ID).
