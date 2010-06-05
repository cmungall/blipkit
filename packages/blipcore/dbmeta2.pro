/* -*- Mode: Prolog -*- */

:- module(dbmeta2,
          [
           (pure)/1,
           op(1150,fx,(pure))
          ]).

pure(Heads):-
	throw(error(context_error(nodirective, pure(Heads)), _)).

:- multifile
	system:term_expansion/2.
:- dynamic
	system:term_expansion/2.

system:term_expansion((:- pure(Pred)),[]).

