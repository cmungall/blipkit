/* -*- Mode: Prolog -*- */

user:dynamic_db(ontol_db).

:- use_module(bio(io)).
:- use_module(bio(ontol_db)).
:- use_module(bio(ontol_reasoner)).
:- use_module(bio(ontol_management)).

:- begin_tests(merge, [setup(load)]).

load:-
        load_biofile(obo,'data/equivtest.obo'),
        find_all_entailments,
        merge_equivalent_classes,
        remove_redundant_facts.

test(foo):-
        write_biofile(obo,_).


:- end_tests(merge).

