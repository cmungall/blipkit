/* -*- Mode: Prolog -*- */

% ----------------------------------------
%  SETUP
% ----------------------------------------

:- ['blip_setup_path.pl'].


% ----------------------------------------
%  CONFIGURATION
% ----------------------------------------
:- (   getenv('BLIP_CONFIG', A)
   ->  concat_atom(L,':',A),
       forall(member(X,L),
              consult(etc(X)))
   ;   true).


:- [etc(bioconf)].
:- [etc(bioconf_rdf)].

% ----------------------------------------
%  PACKAGE SETUP
% ----------------------------------------

% each package has a blipkit module that defines package-specific
% command line processing


:- use_module(bio(blipkit)).
:- use_module(bio(blipkit_ontol)).
:- use_module(bio(blipkit_ontol_metadata)).
:- use_module(bio(blipkit_phylo)).
:- use_module(bio(blipkit_fasta)).
:- use_module(bio(blipkit_sb)).
:- use_module(bio(blipkit_pathway)).
:- use_module(bio(blipkit_web)).
%%:- use_module(bio(blipkit_sql)).
:- use_module(bio(cytoscape_db)).

