:- module(owl2_to_sim,
	  []).

:- use_module(bio(simmatrix_multiset),[]).
:- use_module(bio(tabling)).
:- use_module(bio(index_util)).

:- use_module(library('thea2/owl2_model')).
:- use_module(library('thea2/owl2_graph_reasoner')).

simmatrix_multiset:atomic_subsumed_by(X,Y) :-
	%subClassOfRT(X,Y),
        class_ancestor_over(X,Y1,_),
	\+ exclude(Y1),
        Y=Y1.                   % hack - table class_ancestor_over/3 for all Y


exclude(Class) :-
	atom(Class),
	sub_atom(Class,0,_,_,'http://www.ifomis.org').

exclude('http://ontology.neuinfo.org/NIF/BiomaterialEntities/NIF-GrossAnatomy.owl#birnlex_6'). % anatomical entity
exclude('http://ontology.neuinfo.org/NIF/BiomaterialEntities/NIF-GrossAnatomy.owl#birnlex_4'). % organ
exclude('http://ontology.neuinfo.org/NIF/BiomaterialEntities/NIF-GrossAnatomy.owl#birnlex_16'). % regional part of organ
exclude('http://ontology.neuinfo.org/NIF/BiomaterialEntities/NIF-GrossAnatomy.owl#birnlex_1167'). % Regional part of brain
exclude('http://ontology.neuinfo.org/NIF/BiomaterialEntities/NIF-Molecule.owl#CHEBI_23367'). % molecular entity
exclude('http://ontology.neuinfo.org/NIF/BiomaterialEntities/NIF-Molecule.owl#nlx_mol_20090303'). % molecular role
exclude('http://ontology.neuinfo.org/NIF/BiomaterialEntities/NIF-Subcellular#sao-1337158144'). % Cellular Component
exclude('http://purl.org/obo/owl/PATO#PATO_0001237'). % quality of single physical entity
exclude('http://purl.org/obo/owl/PATO#PATO_0001238'). % quality of related physical entities
exclude('http://purl.org/obo/owl/PATO#PATO_0001241'). % physical object quality
exclude('http://purl.org/obo/owl/PATO#PATO_0000069'). % deviation(from_normal)
exclude(X) :- ontologyAxiom(O,class(X)),\+((subClassOf(X,Y),ontologyAxiom(O,class(Y)))). % exclude root classes
exclude(someValuesFrom(_,X)) :- exclude(X).
%exclude(someValuesFrom(_,someValuesFrom(_,_))). % nesting too deep


% HOOK: simmatrix
%  use the basic tbox reasoner for graph closure
:- multifile simmatrix_multiset:index_hook/1.
simmatrix_multiset:index_hook(reasoner) :-
	materialize_index(owl2_model:equivalent_to(1,1)),
	%table_pred(owl2_tbox_reasoner:subClassOfT/3), % helps?
	%table_pred(owl2_tbox_reasoner:subClassOfT/2).
        true.

