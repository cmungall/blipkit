:- module(bioseq_util, [say_hello/1]).

:- initialization load_foreign_library(bioseq_util).

yo:-
        say_hello(yo).
