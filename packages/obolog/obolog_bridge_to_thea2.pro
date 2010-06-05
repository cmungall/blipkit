/* -*- Mode: Prolog -*- */

:- module(obolog_bridge_to_thea2,
          [
           ]).
:- use_module(library('thea2/owl2_model')).
:- use_module(library('thea2/swrl')).
:- use_module(bio(obolog_db),[]).
%:- rdf_register_ns(oboInOwl,'http://www.geneontology.org/formats/oboInOwl').

% relations

%owl2_model:ontology('http://foo.bar', [annotation('owl:imports','http://www.ifomis.org/bfo/1.1')]).


owl_builtin(instance_of).
owl_builtin(is_a).


owl2_model:class(ID) :- obolog_db:type(ID).
owl2_model:subClassOf(X,Y):- obolog_db:formula(is_a(X,Y)).

owl2_model:subClassOf(Type,allValuesFrom(R,Type)):-
        obolog_db:homeomorphic_for(R,Type).

owl2_model:subClassOf(A,someValuesFrom(R,B)):-
        obolog_db:all_some(RT,R),
        obolog_db:formula(S),
        S=..[RT,A,B].

owl2_model:subClassOf(A,allValuesFrom(RC,complementOf(B))):-
        obolog_db:all_only(RT,R),
        obolog_db:relation_complement_of(R,RC),
        obolog_db:formula(S),
        S=..[RT,A,B].

owl2_model:equivalentClasses([X,intersectionOf([G|Restrs])]) :-
        obolog_db:formula(genus(X,G)),
        findall(Restr,(obolog_db:formula(differentium(X,R,To)),restr(R,To,Restr)),Restrs).

restr(card(RC,N),To,exactCardinality(N,R,To)) :- obolog_db:all_some(RC,R),!.
restr(card(RC,Min,Max),To,Restr) :-
        obolog_db:all_some(RC,R),
        !,
        (   Restr=minCardinality(Min,R,To)
        ;   Restr=maxCardinality(Max,R,To)).
restr(RC,To,someValuesFrom(R,To)) :-
        obolog_db:all_some(RC,R),
        !.
restr(RT,To,allValuesFrom(RC,complementOf(To))):-
        obolog_db:all_only(RT,R),
        obolog_db:relation_complement_of(R,RC),
        !.
restr(card(R,N,N),To,exactCardinality(N,R,To)) :- % GUESS existential
        \+ obolog_db:type_type(R),
        !.                          
restr(card(R,N),To,exactCardinality(N,R,To)) :- % GUESS existential
        \+ obolog_db:type_type(R),
        !.                          
restr(R,To,someValuesFrom(R,To)) :- % GUESS existential
        \+ obolog_db:type_type(R),
        !.
restr(R,To,_) :- throw(restr('cannot translate ~w ~w',[R,To])).


owl2_model:objectProperty(P) :- obolog_db:relation(P).
owl2_model:transitiveProperty(P) :- obolog_db:transitive(P).
owl2_model:symmetricProperty(P) :- obolog_db:symmetric(P).
owl2_model:asymmetricProperty(P) :- obolog_db:asymmetric(P).
owl2_model:antiSymmetricProperty(P) :- obolog_db:anti_symmetric(P).
owl2_model:reflexive(P) :- obolog_db:reflexive(P).
owl2_model:functional(P) :- obolog_db:functional(P).
owl2_model:inverseProperties(P,Q) :- obolog_db:inverse_of(P,Q).
owl2_model:subPropertyOf(P,Q) :- obolog_db:subrelation(P,Q).
owl2_model:subPropertyOf(propertyChain([Q,R]),P) :- obolog_db:holds_over_chain(P,Q,R).

owl2_model:propertyDomain(P,Q) :- obolog_db:domain(P,Q).
owl2_model:propertyRange(P,Q) :- obolog_db:range(P,Q).

owl2_model:disjointClasses([P,Q]) :- obolog_db:disjoint_from(P,Q).
owl2_model:disjointProperties([P,Q]) :- obolog_db:relation_disjoint_from(P,Q).

mmap(label,'http://www.w3.org/2000/01/rdf-schema#label').
mmap(exact_synonym,'oboInOwl:hasExactSynonym').
mmap(broad_synonym,'oboInOwl:hasBroadSynonym').
mmap(narrow_synonym,'oboInOwl:hasNarrowSynonym').
mmap(related_synonym,'oboInOwl:hasRelatedSynonym').
mmap(text_definition,'oboInOwl:hasDefinition').
mmap(homeomorphic_for,'oboInOwl:isHomeomorphicFor').
mmap(comment,'http://www.w3.org/2000/01/rdf-schema#comment').

owl2_model:annotationAssertion(Prop,ID, Lit):-
        mmap(Pred,Prop),
        Axiom=..[Pred,ID,X],
        obolog_db:Axiom,
        atom_literal(X,Lit).

%atom_literal(A,type('http://www.w3.org/2001/XMLSchema#string', A)).
atom_literal(A,literal(A)).

% SWRL

swrl:implies(AL,propertyAssertion(R,i(x),i(y))) :-
        obolog_db:equivalent_to(R,And),And=..[intersection_of|L],findall(propertyAssertion(RI,i(x),i(y)),member(RI,L),AL).



/** <module> maps between obolog model and OWL
  
  ---+ Synopsis
  
==
:- use_module(bio(obolog_bridge_to_thea)).

% 
demo:-
        load_bioresource('ro.obolog'),
        
        nl.
        

==

Command line:

=|bio io-convert -i ro.obolog -to thea -u obolog_bridge_to_thea -o ro.owl|-


  ---+ Details


@author  Chris Mungall
@version $Id$
@see     
@license License


*/
