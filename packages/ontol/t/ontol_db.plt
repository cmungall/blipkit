/* -*- Mode: Prolog -*- */

user:dynamic_db(ontol_db).

:- use_module(bio(io)).
:- use_module(bio(ontol_db)).
:- use_module(bio(ontol_reasoner)).
:- use_module(bio(ontol_management)).

:- begin_tests(fly, [setup(load)]).

load:-
        load_bioresource(fly_anatomy).

test(parent_over_nr):-
        class(C,'head sensillum'),      
        solutions(P,closure(ontol_db:parent_over_nr(part_of),C,P),Ps),
        writeln(parents=Ps),
        nl,
        member('FBbt:00000001',Ps).

test(graph):-
        class(TestC,'head sensillum'),
        closure_to_edgelist_delta(ontol_db:parent_over_nr(part_of),'FBbt:00000001',Edges,down(2)),
        writeln(edges=Edges),
        solutions(C,member(edge(C,_,_),Edges),Cs),
        writeln(cs=Cs),
        nl,
        member(TestC,Cs).

:- end_tests(fly).


:- begin_tests(sofa, [setup(load)]).

load:-
        load_biofile(obo,'sofa.obo').

test(sofa):-
        findall(PID,subclassT('SO:0000234',PID),PIDs),
        writeln(pidsA=PIDs),
        nl,
        member('SO:0000673',PIDs).

:- end_tests(sofa).


:- begin_tests(po, [setup(load)]).

load:-
        load_biofile(go,'parent_over_test.go').

test(t1, true(parentT('Y:4','X:1'))).
test(t2, true(parent_over(part_of,'Y:3','X:3'))).
test(t3, true(parent_over(part_of,'Y:4','X:1'))).
test(t4, all(parent_over_nr(part_of,'Y:4',SuperPart),['X:3'])).
test(t5) :-
        findall(PID,subclassT('SO:0000234',PID),PIDs),
        writeln(pidsA=PIDs),
        nl,
        member('SO:0000673',PIDs).

:- end_tests(po).

