/* -*- Mode: Prolog -*- */

:- module(thea_wrapper,
          [
	   thea_parse/1,
	   thea_parse/2
           ]).
:- use_module(bio('thea/owl_parser')).
:- use_module(bio('thea/owl_as2rdf')).
:- use_module(bio('thea/owl_reasoner')).

thea_parse(F):-
	thea_parse(F,[]).
thea_parse(F,Opts):-
	(   member(imports(Imports),Opts)
	->  true
	;   Imports=false),
	(   member(clear(Clear),Opts)
	->  true
	;   Clear=false),
	owl_parse(F,Clear,Clear,Imports),
	debug(owl_parser,'parsed ~w',[F]).

