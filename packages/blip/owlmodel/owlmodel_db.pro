/* -*- Mode: Prolog -*- */

:- module(owlmodel_db,[
                       ]).

/*
  http://www.w3.org/TR/owl2-syntax/

  */

/*
  ANNOTATIONS

  e.g.
  'SubObjectPropertyOf' '(' { annotation } subObjectPropertyExpression objectPropertyExpression ')'

  
  */

%%  entityAnnotation(?Entity,?Annotation)
:- extensional(entityAnnotation/2).


/*
  CLASS AXIOMS
  */

%% subClassOf(?SubDesc,?SuperDesc)
:- extensional(subClassOf/2).

%% equivalentClasses(?DescList)
:- extensional(equivalentClasses/1).

%% disjointClasses(?DescList)
:- extensional(disjointClasses/1).

%% disjointUnion(?ClassURI,?DescList)
:- extensional(disjointUnion/2).

/*
  PROPERTY AXIOMS
  */

%% subObjectPropertyOf(?SubObjectPropertyExpression,?ObjectPropertyExpression)
:- extensional(subObjectPropertyOf/2).

%% equivalentObjectProperties(?ObjectPropertyExpressionList)
:- extensional(equivalentObjectProperties/2).

%% disjointObjectProperties(?ObjectPropertyExpressionList)
:- extensional(disjointObjectProperties/2).

%% objectPropertyDomain(?ObjectPropertyExpression,?Desc)
:- extensional(objectPropertyDomain/2).

%% objectPropertyRange(?ObjectPropertyExpression,?Desc)
:- extensional(objectPropertyRange/2).

%% inverseObjectProperties(?ObjectPropertyExpression,?ObjectPropertyExpression)
:- extensional(inverseObjectProperties/2).

/*
  DATA PROPERTY AXIOMS
  */

%% subDataPropertyOf(?SubDataPropertyExpression,?DataPropertyExpression)
:- extensional(subDataPropertyOf/2).

%% equivalentDataProperties(?DataPropertyExpressionList)
:- extensional(equivalentDataProperties/2).

%% disjointDataProperties(?DataPropertyExpressionList)
:- extensional(disjointDataProperties/2).

%% objectPropertyDomain(?DataPropertyExpression,?Desc)
:- extensional(objectPropertyDomain/2).

%% objectPropertyRange(?DataPropertyExpression,?DataRange)
% dataRange := datatypeURI | dataComplementOf | dataOneOf | datatypeRestriction
% dataComplementOf := 'DataComplementOf' '(' dataRange ')'
% dataOneOf := 'DataOneOf' '(' constant { constant } ')'
% datatypeFacet :=
%     'length' | 'minLength' | 'maxLength' | 'pattern' |
%     'minInclusive' | 'minExclusive' | 'maxInclusive' | 'maxExclusive' |
%     'totalDigits' | 'fractionDigits'
% restrictionValue := constant
% datatypeRestriction := 'DatatypeRestriction' '(' dataRange datatypeFacet restrictionValue { datatypeFacet restrictionValue } ')'
:- extensional(objectPropertyRange/2).

%% inverseDataProperties(?DataPropertyExpression,?DataPropertyExpression)
:- extensional(inverseDataProperties/2).


/*
  DESCRIPTIONS

  entity := datatype | owlClass | objectProperty | dataProperty | individual
datatype := 'Datatype' '(' datatypeURI ')'
owlClass := 'OWLClass' '(' owlClassURI ')'
objectProperty := 'ObjectProperty' '(' objectPropertyURI ')'
dataProperty := 'DataProperty' '(' dataPropertyURI ')'
individual := 'Individual' '(' individualURI ')'

  
  objectUnionOf := 'ObjectUnionOf' '(' description description { description } ')'
objectIntersectionOf := 'ObjectIntersectionOf' '(' description description { description } ')'
objectComplementOf := 'ObjectComplementOf' '(' description ')'
objectOneOf := 'ObjectOneOf' '(' individualURI { individualURI }')'

  objectAllValuesFrom := 'ObjectAllValuesFrom' '(' objectPropertyExpression description ')'
objectSomeValuesFrom := 'ObjectSomeValuesFrom' '(' objectPropertyExpression description ')'
objectExistsSelf := 'ObjectExistsSelf' '(' objectPropertyExpression ')'
objectHasValue := 'ObjectHasValue' '(' objectPropertyExpression individualURI ')'

  cardinality := nonNegativeInteger
objectMinCardinality := 'ObjectMinCardinality' '(' cardinality objectPropertyExpression [ description ] ')'
objectMaxCardinality := 'ObjectMaxCardinality' '(' cardinality objectPropertyExpression [ description ] ')'
objectExactCardinality := 'ObjectExactCardinality' '(' cardinality objectPropertyExpression [ description ] ')'
  
  */









