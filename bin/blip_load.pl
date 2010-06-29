/* -*- Mode: Prolog -*- */

% ----------------------------------------
%  SETUP
% ----------------------------------------

% all blip prolog files use .pro suffix
user:prolog_file_type(pro,prolog).

% setup "bio" shortcut.
% eventually this will be deprecated and
% library(PACKAGE/MODULE) will be used
user:file_search_path(bio, blipkit(attic)).
user:file_search_path(bio, blipkit(biblio)).
user:file_search_path(bio, blipkit(bionlp)).
user:file_search_path(bio, blipkit(blipcore)).
user:file_search_path(bio, blipkit(curation)).
user:file_search_path(bio, blipkit(genomic)).
user:file_search_path(bio, blipkit(homol)).
user:file_search_path(bio, blipkit(metadata)).
user:file_search_path(bio, blipkit(obolog)).
user:file_search_path(bio, blipkit(omim)).
user:file_search_path(bio, blipkit(graph)).
user:file_search_path(bio, blipkit(ontol)).
user:file_search_path(bio, blipkit(ontol/conf)).
user:file_search_path(bio, blipkit(geo)).
user:file_search_path(bio, blipkit(pheno)).
user:file_search_path(bio, blipkit(phylo)).
user:file_search_path(bio, blipkit(rdft)).
user:file_search_path(bio, blipkit(sb)).
user:file_search_path(bio, blipkit(sb/conf)).
user:file_search_path(bio, blipkit(serval)).
user:file_search_path(bio, blipkit(sql)).
user:file_search_path(bio, blipkit(stats)).
user:file_search_path(bio, blipkit(structure)).
user:file_search_path(bio, blipkit(variation)).
user:file_search_path(bio, blipkit(web)).
user:file_search_path(bio, blipkit(xml)).

% APPS:
user:file_search_path(bio, blipkit(obol)).

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
:- use_module(bio(blipkit_phylo)).
:- use_module(bio(blipkit_fasta)).
:- use_module(bio(blipkit_sb)).
:- use_module(bio(blipkit_pathway)).
:- use_module(bio(blipkit_web)).
:- use_module(bio(blipkit_sql)).


