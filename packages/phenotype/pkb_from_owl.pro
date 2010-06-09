:- module(pkb_from_owl,[]).

:- use_module(library('thea2/owl2_model')).
:- use_module(library('thea2/owl2_basic_reasoner')).
:- use_module(pkb_db).
:- use_module(bio(tabling)).

:- table_pred(owl2_basic_reasoner:entailed/1).

literal_text(literal(type(_,X)),X).

pkb_db:species(S) :- species_label(S,_).
pkb_db:species_label('http://ontology.neuinfo.org/NIF/BiomaterialEntities/NIF-Organism.owl#birnlex_516',human).
pkb_db:species_label('http://ontology.neuinfo.org/NIF/BiomaterialEntities/NIF-Organism.owl#birnlex_167',mouse).
pkb_db:species_label('http://ontology.neuinfo.org/NIF/BiomaterialEntities/NIF-Organism.owl#birnlex_535',fruitfly). % DM
pkb_db:species_label('http://ontology.neuinfo.org/NIF/BiomaterialEntities/NIF-Organism.owl#birnlex_147',fruitfly). % D
pkb_db:species_label('http://ontology.neuinfo.org/NIF/BiomaterialEntities/NIF-Organism.owl#birnlex_160',rat). 
foo('0').

pkb_db:organism(Org) :- entailed(classAssertion('http://ontology.neuinfo.org/NIF/BiomaterialEntities/NIF-Organism.owl#birnlex_2',Org)). % Organism
pkb_db:organism(Org) :- entailed(subClassOf(Org,'http://ontology.neuinfo.org/NIF/BiomaterialEntities/NIF-Organism.owl#birnlex_516')). % human

pkb_db:organism_species(Org,Species) :- species_label(Species,_),entailed(subClassOfReflexive(Org,Species)).
pkb_db:organism_species(Org,Species) :- species_label(Species,_),entailed(classAssertion(Species,Org)).

:- table_pred(pkb_db:organism/1).

pkb_db:organism_label(Org,X) :- organism(Org),labelAnnotation_value(Org,X).

pkb_db:organism_type(Org,Type) :- organism(Org),subClassOf(Org,Type),class(Type).
pkb_db:organism_type(Org,Type) :- organism(Org),subClassOf(Org,intersectionOf(L)),member(Type,L),class(Type).   % e.g. Human_with_X < Human and ...
pkb_db:organism_type(Org,Type) :- organism(Org),equivalent_to(Org,intersectionOf(L)),member(Type,L),class(Type).   % e.g. Human_with_X == Human and ...
pkb_db:organism_type(Org,Type) :- organism(Org),classAssertion(Type,Org).

pkb_db:organism_role(Org,model) :- organism(Org),classAssertion(_,Org).
pkb_db:organism_role(Org,patient) :- organism(Org),subClassOf(Org,_).

pkb_db:organism_description(Org,Desc) :-
        organism(Org),
        propertyAssertion('http://purl.org/obo/owl/obo#inheres_in',P,Org),
        propertyAssertion('http://purl.org/dc/elements/1.1/source',P,DescLiteral),literal_text(DescLiteral,Desc).

%% equivalentClasses(['http://ccdb.ucsd.edu/SAO/Disease/1.0/NDPO.owl#Human_with_Multiple_System_Atrophy', intersectionOf(['http://ontology.neuinfo.org/NIF/BiomaterialEntities/NIF-Organism.owl#birnlex_516', someValuesFrom('http://ontology.neuinfo.org/NIF/Backend/BIRNLex-OBO-UBO.owl#birnlex_17', 'http://ontology.neuinfo.org/NIF/Dysfunction/NIF-Dysfunction.owl#birnlex_12573')])]).
pkb_db:disease(D) :-
        equivalent_to(_,intersectionOf(L)),
        member(someValuesFrom('http://ontology.neuinfo.org/NIF/Backend/BIRNLex-OBO-UBO.owl#birnlex_17', D),L),
        entailed(subClassOf(D,'http://ontology.neuinfo.org/NIF/Dysfunction/NIF-Dysfunction.owl#birnlex_12796')).

pkb_db:organism_role_disease(O,Role,D) :-
        equivalent_to(O1,intersectionOf(L)),
        member(someValuesFrom('http://ontology.neuinfo.org/NIF/Backend/BIRNLex-OBO-UBO.owl#birnlex_17', D),L),
        entailed(subClassOf(D,'http://ontology.neuinfo.org/NIF/Dysfunction/NIF-Dysfunction.owl#birnlex_12796')),
        (   O1=O,
	    Role=canonical
        ;   classAssertion(O1,O),
	    Role=patient).


pkb_db:disease_label(D,X) :- disease(D),labelAnnotation_value(D,X).

pkb_db:disease_description(Disease,Desc) :-
        disease(Disease),
        (   P='http://www.w3.org/2004/02/skos/core#definition'
        ;   P='http://ontology.neuinfo.org/NIF/Backend/BIRNLex_annotation_properties.owl#birnlexDefinition'),
        anyPropertyAssertion(P,
                             Disease,
                             DescLiteral),
        literal_text(DescLiteral,Desc).

% phenotype attached directly to organism
% here the quad is a 4-tuple of *individuals*. this is then later used by organism_phenotype/2
% todo - manufacture fake inst ID
pkb_db:organism_phenotype_inst(Org,((-),Q,D,(-))) :-
        organism(Org),
	% Org bearer_of Q towards D 
        propertyAssertion('http://ontology.neuinfo.org/NIF/Backend/BIRNLex-OBO-UBO.owl#birnlex_17',Org,Q),
        \+ classAssertion('http://ontology.neuinfo.org/NIF/DigitalEntities/NIF-Investigation.owl#birnlex_2087',Q), % fake Phenotype class
        (   propertyAssertion('http://purl.org/obo/owl/obo#towards', Q, D)
        ->  true
        ;   D=(-)).


% relational qualifiers
pkb_db:organism_phenotype_inst(Org,(E,Q,D,(-))) :-
        organism(Org),
	% Org has_part E bearer_of Q towards D 
        propertyAssertion('http://www.obofoundry.org/ro/ro.owl#has_part',Org,E),
        propertyAssertion('http://ontology.neuinfo.org/NIF/Backend/BIRNLex-OBO-UBO.owl#birnlex_17',E,Q), % bearer-of
        (   propertyAssertion('http://purl.org/obo/owl/obo#towards', Q, D)
        ->  true
        ;   D=(-)).

% double parthood
pkb_db:organism_phenotype_inst(Org,(E,Q,D,W)) :-
        organism(Org),
        propertyAssertion('http://www.obofoundry.org/ro/ro.owl#has_part',Org,W),
        propertyAssertion('http://www.obofoundry.org/ro/ro.owl#has_part',W,E),
        propertyAssertion('http://ontology.neuinfo.org/NIF/Backend/BIRNLex-OBO-UBO.owl#birnlex_17',E,Q),
        (   propertyAssertion('http://purl.org/obo/owl/obo#towards', Q, D)
        ->  true
        ;   D=(-)).

pkb_db:organism_phenotype(Org,P) :-
	opq(Org,P,_).
pkb_db:phenotype_quad(P,PQ) :-
	setof(P-PQ,Org^opq(Org,P,PQ),PPQs),
	member(P-PQ,PPQs).
	
nullableClassAssertion(C,I) :- classAssertion(C,I),!.
nullableClassAssertion(-,-).

% instance-level
opq(O,P,PQ) :-
	PQ = (E,Q,D,W),
	P = (E1,Q1,D1,W1),
        organism_phenotype_inst(O,P),
        nullableClassAssertion(Q,Q1),
	\+ has_number_of(Q),
        nullableClassAssertion(E,E1),
        nullableClassAssertion(D,D1),
        nullableClassAssertion(W,W1).


% rewrite has_number_of
% e.g. has_number_of (chromatin bearer_of q)
opq(O,P,PQ) :-
	PQ = (E,Q,D,W),
	P = (E1,NQ1,D1,_),
        organism_phenotype_inst(O,P),
        nullableClassAssertion(NQ,NQ1),
	has_number_of(NQ),
	% shift to E
        nullableClassAssertion(E,D1),
        nullableClassAssertion(W,E1),
	propertyAssertion('http://ontology.neuinfo.org/NIF/Backend/BIRNLex-OBO-UBO.owl#birnlex_17',D1,Q1),
        nullableClassAssertion(Q,Q1),
        (   propertyAssertion('http://purl.org/obo/owl/obo#towards', Q1, NestedD1)
	->  nullableClassAssertion(D,NestedD1)
	;   D=(-)).

% class-level (e.g. Human With Parkinsons)
opq(Org,P,PQ) :-
	PQ = (E,Q,D,W),
        % todo - split this on P
        subClassOf(Org,
                   someValuesFrom('http://ontology.neuinfo.org/NIF/Backend/BIRNLex-OBO-UBO.owl#birnlex_17',
                                  P)),
        equivalent_to(P,PCE),
        PCE=intersectionOf(CEL),
        select(Q,CEL,DiffL),        
        entailed(subClassOf(Q,'http://purl.org/obo/owl/PATO#PATO_0000001')),
	\+ has_number_of(Q),
        differentiae_bearer(DiffL,E,W),
        %class(E),
        %class(W),
        differentiae_towards(DiffL,D).
        %class(D).

% Ex: Locus ceruleus has_number_of decreased-diameter Mitochondrion
% ==> LC has_part Mt that has_q dd
opq(Org,P,PQ) :-
        subClassOf(Org,
                   someValuesFrom('http://ontology.neuinfo.org/NIF/Backend/BIRNLex-OBO-UBO.owl#birnlex_17',
                                  P)),
        equivalent_to(P,PCE),
        PCE=intersectionOf(CEL),
	has_number_of(NumberOf),
        select(NumberOf,CEL,DiffL), % has number of
        differentiae_bearer(DiffL,W,_),
        differentiae_towards(DiffL,intersectionOf(DEL)),
	select(E,DEL,D_DiffL),
	memberchk(someValuesFrom('http://ontology.neuinfo.org/NIF/Backend/BIRNLex-OBO-UBO.owl#birnlex_17',Q),D_DiffL),
	PQ = (E,Q,-,W).

has_number_of('http://purl.org/obo/owl/PATO#PATO_0001555').


differentiae_bearer(DiffL,E,-) :-
        member(someValuesFrom('http://purl.org/obo/owl/obo#inheres_in',E),DiffL),
        class(E),
        !.
differentiae_bearer(DiffL,E,W) :-
        member(someValuesFrom('http://purl.org/obo/owl/obo#inheres_in',E1),DiffL),
        E1=intersectionOf(EIL), % assume length=2
        (   member(E,EIL),
            class(E),
            (   member(someValuesFrom('http://www.obofoundry.org/ro/ro.owl#has_part',W),EIL) % Todo
            ;   member(someValuesFrom('http://purl.org/obo/owl/OBO_REL#has_part',W),EIL))
        ->  true
        ;   member(E,EIL),
            class(E),
            member(someValuesFrom('http://purl.org/obo/owl/OBO_REL#part_of',W),EIL)),
        !.
differentiae_bearer(DiffL,E,-) :-
        member(someValuesFrom('http://purl.org/obo/owl/obo#inheres_in',E),DiffL),
        E=intersectionOf(_),
        !.
differentiae_bearer(_,(-),(-)).

differentiae_towards(DiffL,D) :-
        member(someValuesFrom('http://purl.org/obo/owl/obo#towards',D),DiffL),
        !.
differentiae_towards(_,-).
        





/** <module> 

  ---+ Synopsis

==
:- use_module(bio(pkb_from_owl)).

% 
demo:-
  nl.
  

==

---+ Details

TODO: should be named nif_owl


*/
