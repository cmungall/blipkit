/* -*- Mode: Prolog -*- */

:- use_module(bio(kegg_db)).
:- use_module(bio(kegg_xml)).

:- begin_tests(kegg, []).

test(alz):-
	load_biofile(keggxml,'test_data/hsa05010.xml'),
	write_biofile(kegg_db:pro).


:- end_tests(enscore).
