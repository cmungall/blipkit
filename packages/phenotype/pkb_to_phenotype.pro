:- module(pkb_to_phenotype,[]).

:- use_module(pkb_db).
:- use_module(phenotype_db).
:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).
:- use_module(library('thea2/owl2_basic_reasoner')).
:- use_module(library('thea2/owl2_model'),[]).


phenotype_db:feature_phenotype(O,P) :- organism_phenotype_quad(O,P), \+ hide_phenotype(P).

% also include organism type
phenotype_db:feature_phenotype(T,P) :- setof(T,O^organism_type(O,T),Ts),member(T,Ts),\+species(T),organism_type(O,T),organism_phenotype(O,P).

% temp TODO - move?
ontol_db:subclass(X,Y) :- entailed(subClassOfReflexive(X,Y)),Y\='http://www.ifomis.org/bfo/1.1/snap#Object'.
ontol_db:restriction(X,part_of,Y) :- entailed(subClassOf(X,someValuesFrom(R,Y))),part_of(R),atom(X),atom(Y).
%ontol_db:restriction(X,part_of,Y) :- subclass(X,Y). % reflexivity

metadata_db:entity_label(X,V) :- owl2_model:labelAnnotation_value(X,V).

part_of('http://www.obofoundry.org/ro/ro.owl#part_of').
part_of('http://purl.org/obo/owl/obo#part_of').
part_of(part_of).
part_of('http://ontology.neuinfo.org/NIF/BiomaterialEntities/NIF-Neuron-NT-Bridge.owl#soma_located_in').

hide_phenotype( (_,'http://purl.org/obo/owl/PATO#PATO_0001905',_,_) ).





