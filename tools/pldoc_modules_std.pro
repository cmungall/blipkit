%:- use_module(bio(docroot)).
:- use_module(bio(io)).
:- use_module(library(pldoc)).

:- [bio(load_blipcore)].
:- [bio(load_sql)].
:- [bio(load_curation)].
:- [bio(load_sb)].
:- [bio(load_phylo)].
:- [bio(load_ontol)].
:- [bio(load_genomic)].
:- [bio(load_web)].
:- [bio(load_ext)].
:- [bio(load_obol)].

:- use_module(bio(dotwriter)).
:- use_module(library(semweb/rdfs)).

% Thea2
:- use_module(library('thea2/owl2_model')).
:- use_module(library('thea2/owl2_from_rdf')).
%:- use_module(library('thea2/owl2_plsyn')).
:- use_module(library('thea2/owl2_xml')).
:- use_module(library('thea2/owl2_io')).
:- use_module(library('thea2/owl2_to_prolog_dlp')).
:- use_module(library('thea2/owl2_to_progol')).
:- use_module(library('thea2/swrl')).
:- use_module(library('thea2/owl2_util')).
:- use_module(library('thea2/owl2_reasoner')).
:- use_module(library('thea2/prolege_server')).
:- use_module(library('thea2/owlgres/owl2_sqlmap_owlgres')).

user:file_search_path(pldoc, '/users/cjm/cvs/blipkit/doc').
