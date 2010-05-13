/* -*- Mode: Prolog -*- */

:- use_module(seqfeature_db).
:- use_module(parser_gff3).
:- use_module(bio(io)).

/*

  To run these tests, start up swipl and execute the following:

  ==
  ['intervaldb.plt'].
  set_prolog_flag(verbose,normal),
  set_test_options([silent(false)]),
  run_tests.
  ==

  */

:- begin_tests(gff, [setup(load_test_data)]).

load_test_data :-
        load_biofile('test_data/dmel4.gff3').


test(ok):-
        format('**OK**~n').


test(x):-
        forall(feature(F),
               writeln(F)).


:- end_tests(gff).


:- begin_tests(t2, []).

test(foo):- writeln(xxx).


:- end_tests(t2).


