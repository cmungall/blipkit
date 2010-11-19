

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
exclude(X) :- % exclude root classes. a root class is a class that has no superclass in the same ontology
        forall(ontologyAxiom(O,class(X)), % note some classes are multiply declared - a single intra-ontology subclass is sufficient for non-root status
               \+((subClassOf(X,Y),ontologyAxiom(O,class(Y))))).
exclude(someValuesFrom(_,X)) :- exclude(X).
%exclude(someValuesFrom(_,someValuesFrom(_,_))). % nesting too deep
