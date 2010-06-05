/* -*- Mode: Prolog -*- */

:- module(commonlogic_model,
	  [
	   
	  ]).
:- require([ is_list/1
	   , current_prolog_flag/2
	   , forall/2
	   , debug/3
	   ]).


:- use_module(library(lists)). % Yap

% The ext/1 directive is used to declare a predicate extensional. Extensional predicates are dynamic, because we may
% wish to modify the database at run time. They are multifile, as we may wish to load from multiple sources.
% In tabled prologs such as Yap, extensional predicates are tabled, because they may be entailed as well as asserted.
system:term_expansion((:- ext(Pred)),
                    [(   :- multifile Pred),(:- dynamic Pred),axiompred(Pred)]) :- current_prolog_flag(dialect,swi).

system:term_expansion((:- ext(Pred)),
                    [(:- table(Pred)),(:- multifile Pred),axiompred(Pred)]) :- current_prolog_flag(dialect,yap).

/****************************************
  AXIOMS
  ****************************************/

