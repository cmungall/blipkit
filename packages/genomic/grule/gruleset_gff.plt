/* -*- Mode: Prolog -*- */

:- use_module(bio(seqfeature_db)).
:- use_module(bio(genome_db)).
:- use_module(bio(genome_bridge_from_seqfeature)).
:- use_module(bio(parser_gff3)).
:- use_module(bio(io)).
:- [gruleset_gff].


:- begin_tests(rules, [setup(load_test_data)]).

load_test_data :-
        load_biofile('test_data/dmel4alltypes.gff3').


test(variant_intersects):-
        forall((sequence_variant(V),
                feature_intersects(V,X)),
               writeln(V-X)).

test(intron_intersects):-
        forall((intron(V),
                feature_intersects(V,X)),
               writeln(V-X)).

test(variant_intersects_intron):-
        forall((sequence_variant(V),
                feature_intersects(V,X),
                intron(X)),
               writeln(V-X)).

test(variant_intersects_splice_site):-
        forall((splice_site(V),
                feature_intersects(V,X),
                intron(X)),
               writeln(V-X)).


xxtest(x):-
        forall(feature(F),
               writeln(F)).
xxtest(x):-
        nl,
        write_biofile(genome_db:pro,_).



:- end_tests(rules).



