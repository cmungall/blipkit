/* -*- Mode: Prolog -*- */

:- module(unittest,
          [
           run_tests/2,
           test_summary/0
           ]).

:- dynamic test_results/1.
:- dynamic testdata_loaded/2.
:- use_module(bio(bioprolog_util)).

load_testdata(ID):-
        load_testdata(ID,_).
load_testdata(ID,Data):-
        (testdata_loaded(ID,Data)
        ->  true
        ;   on_start(ID,Data),
            assert(testdata_loaded(ID,Data))),
        !.
load_testdata(ID,_):-
        throw(load_testdata(ID)).

% Test may be unground - in which case, run all
run_tests(ModTerm,Test):-
        ModTerm =.. [_,Mod],
        ensure_loaded(ModTerm),
        catch(Mod:setof(Unit,unittest(Unit),Units),
              E,
              format('No tests defined for ~w err: ~w~n',[Mod,E])),
        (var(E)
        ->  flatten(Units,UnitsFlat),
            setof(Test,run_tests1(Mod,UnitsFlat,Test),_)
        ;   true).

run_tests1(Mod,Unit,ID):-
        member(TestTerm,Unit),
        TestTerm=test(ID,_,_,_),
        format('running test: [~w] ~w~n',[Mod,ID]),
        catch(run_test(Mod-ID,Unit,TestTerm),
              E,
              fail_test(Mod,ID,E)).

run_pre(_,[]).
run_pre(Unit,[Var=FuncCall|Pres]):-
        member(FuncCall=Code/Var,Unit),
        Code,
        !,
        run_pre(Unit,Pres).

run_test(Mod-ID,Unit,test(ID,Pre,Code,Cond)):-
        debug(unittest,' test preconditions for ~w = ~w:',[Unit,Pre]),
        (   run_pre(Unit,Pre)
        ->  true
        ;   format(user_error,'** test preconditions failed **~n',[]),
            fail),
        debug(unittest,' code ~w:',[Code]),
        call_with_time(Code,Time),
        format('test ~w ran in: ~w~n',[ID,Time]),
        !,
        (Cond
        ->  pass_test(Mod,ID)
        ;   fail_test(Mod,ID,Cond)).
run_test(Mod-ID,_,_):-
        fail_test(Mod,ID,goal_failed).

pass_test(Mod,Test):-
        format('PASS: ~w ~w~n',[Mod,Test]),
        assert(test_results(pass(Mod,Test))).
fail_test(Mod,Test,Cond):-
        format('FAIL: ~w ~w [reason: expected ~w]~n',[Mod,Test,Cond]),
        assert(test_results(fail(Mod,Test))).

test_summary:-
        format('~n------------------~n',[]),
        (setof(Mod-Test,test_results(fail(Mod,Test)),Fails)
        ->  length(Fails,NumFails),
            format('TEST SUITE FAILS~nNum failures:~w~n',[NumFails]),
            writeln(Fails)
        ;   format('All tests pass!~n')).

call_with_time(Goal,Time):-
        statistics(cputime,T1),
        Goal,
        statistics(cputime,T2),
        Time is T2-T1.

/** <module> unit tests DEPRECATED - use plunit instead

  ==
  :- module(foo,[add/3]).
  
  add(A,B,C):- C is A+b.

  % -------------------- TESTS --------------------

  unittest(test(add_test,                % -- ID of test
              [],                      % -- init function calls
              (add(2,3,X)),            % -- test goal
              (X=5))).                 % -- test post-conditions
  ==

  ---+ Description

  This module allows unit tests to be defined on a per-module basis,
by using the unittest/1 predicate inside the module to be tested. An
entire package can be tested using an external script, eg
tools/run-tests or "make test"

  See blip modules for details

  ---++ FUTURE

  now unit tests are available as part of swi-prolog, this package may become deprecated. See:
  
  http://www.swi-prolog.org/packages/unittest.html

  (same name, totally different module)
  
  */
