/* -*- Mode: Prolog -*- */

:- module(obolog_bridge_to_thea,
          [
           ]).
:- use_module(bio('thea/owl_parser')).
:- use_module(bio(obolog_db)).
%:- rdf_register_ns(oboInOwl,'http://www.geneontology.org/formats/oboInOwl').

% relations

/*
realizeall:-
        forall(relation(ID),realize(ID)).

realize(ID):-
        findall(Axiom,owl(ID,Axiom),Axioms),
        owl_parser:maplist(assert,Axioms).
*/

%owl_parser:ontology('http://foo.bar', [annotation('owl:imports','http://www.ifomis.org/bfo/1.1')]).

subrelation_chain(R,subObjectPropertyChain([R1,R2])):-
        holds_over_chain(R,R1,R2).
subrelation_chain(R,subObjectPropertyChain([R1,R2])):-
        equivalent_to_chain(R,R1,R2).

owl_builtin(instance_of).
owl_builtin(is_a).

owl_parser:property(ID,false,Annotations,Supers,Props,Domains,Ranges):-
        relation(ID),
        \+ owl_builtin(ID), \+ ((label(ID,N),owl_builtin(N))),
        annotations(ID,Annotations),
        findall(Super,(   subrelation(ID,Super)
                      ;   subrelation_chain(ID,Super)),Supers),
        properties(ID,Props),
        findall(Domain,domain(ID,Domain),Domains),
        findall(Range,range(ID,Range),Ranges).

owl_parser:class(ID,false,false,Annotations,Axioms):-
        type(ID),
        annotations(ID,Annotations),
        Axioms=[].

owl_parser:subclassOf(Type,restriction(R,allValuesFrom(Type))):-
        homeomorphic_for(R,Type).
owl_parser:subclassOf(X,Y):- formula(is_a(X,Y)).

properties(ID,[objectProperty,_F,_IF,T,S,iof(Inv)]):-
        setif(transitive(ID),T),
        setif(symmetric(ID),S),
        (   inverse_of(ID,Inv) -> true ; true).

annotations(ID,Annots):-
        findall(Annot,annotation(ID,Annot),Annots).

mmap(label,'rdfs:label').
mmap(exact_synonym,'oboInOwl:hasExactSynonym').
mmap(broad_synonym,'oboInOwl:hasBroadSynonym').
mmap(narrow_synonym,'oboInOwl:hasNarrowSynonym').
mmap(related_synonym,'oboInOwl:hasRelatedSynonym').
mmap(text_definition,'oboInOwl:hasDefinition').
mmap(homeomorphic_for,'oboInOwl:isHomeomorphicFor').
mmap(comment,'rdfs:comment').

annotation(ID,annotation(Prop, Lit)):-
        mmap(Pred,Prop),
        Axiom=..[Pred,ID,X],
        Axiom,
        atom_literal(X,Lit).

%atom_literal(A,type('http://www.w3.org/2001/XMLSchema#string', A)).
atom_literal(A,literal(A)).

setif(Goal,Var):-
        (   Goal
        ->  Var=true
        ;   true).


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
