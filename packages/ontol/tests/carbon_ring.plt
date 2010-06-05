/* -*- Mode: Prolog -*- */

user:dynamic_db(ontol_db).
user:dynamic_db(metadata_db).

:- use_module(bio(io)).
:- use_module(bio(ontol_db)).
:- use_module(bio(ontol_reasoner)).

:- begin_tests(cr, [setup(load)]).

load:-
        load_biofile(obo,'test_data/carbon_ring.obo'),
        find_all_entailments.

test(foo):-
        write_biofile(obo,_).


:- end_tests(cr).

