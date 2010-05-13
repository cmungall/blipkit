
/* -*- Mode: Prolog -*- */



:- module(ontograph_db,
          []).


:- use_module(bio(dbmeta)).

%%   node(?Node) is nondet
%
%    A node in a graph: represents a class, relation or instance
%    A node identifer can be an atom or a prolog term
:- extensional node/1.

%%   node_metatype(?Node,?Metatype) is nondet
%%   node_metatype(+Node,?Metatype) is semidet
%
%    nodes can optionally be assigned to a dataclass in the ontograph model
%
%    Options:
%        * class
%        * relation
%        * instance
:- extensional node_metatype/2.

%%   node_label(?Node,?Atom) is nondet
%
%    A label intended for humans
%     If we later need to represent xml:lang etc, we can add an extra predicate that uses
%     the pair as an ID
:- extensional node_label/2.

%%   node_source(?Node,?SourceNode) is nondet
%
%    arg4 in RDF quad?
:- extensional node_source/2.

%%   node_metaproperty(?Node,?BooleanProperty) is nondet
%
%    extensible boolean properties of the node
%
%    Options:
%        * reflexive (relations only)
%        * transitive (relations only)
%        * symmetric (relations only)
%        * anti_symmetric (relations only)
%        * anonymous (class and instance nodes only)
:- extensional node_metaproperty/2.

%%   node_tagval(?Node,?DatatypeRelation,?Val) is nondet
%
%    extensible slot-value system
%    equivalent to triples for datatype values in RDF/OWL
%
%    Options:
%        * reflexive (relations only)
%        * transitive (relations only)
%        * symmetric (relations only)
%        * anti_symmetric (relations only)
%        * anonymous (class and instance nodes only)
:- extensional node_tagval/3.

%%   link(?Link,?Node,?Relation,?Target,?IsNegated,?Combinator) is nondet
%
%    typed link between two nodes in an ontograph
:- extensional link/6.


res(R):- node(R).
res(R):- link(R,_,_,_).
link(R):- link(R,_,_,_).

%% link_tagval(?Link,?Tag,?Val) is nondet
%link_tagval(Link,T,V):- link(Link),node_tagval(Link,T,V).
        
node_label(N,L):- node_label(N,L,_).
reflexive_relation(R):- node_metaproperty(R,reflexive).
transitive_relation(R):- node_metaproperty(R,transitive).
symmetric_relation(R):- node_metaproperty(R,symmetric).
anti_symmetric_relation(R):- node_metaproperty(R,anti_symmetric).

relation_inverse_of(R,RI):- link(_,R,owl:inverseOf,RI).

anonymous_node(R):- node_metaproperty(R,anonymous).

node_description(N,D):- node_tagval(N,dc:description,D).
node_alias(N,D):- node_tagval(N,alias,D).

universal(R):- node_metatype(R,class).
relation(R):- node_metatype(R,relation).
instance(R):- node_metatype(R,instance).
/** <module>
  @author Chris Mungall
  @version  $Revision$
  @date  $Date$
  @license LGPL

  ---+ Name
  ---++ ontograph_db
- 

  ---+ Synopsis

  ==
  :- use_module(bio(ontograph_db)).

  ==

  ---+ Description

  open Qs:

  use for everything?
  bridge layers - re-interpret existing schemas

  or a more meta-programming approach - macros to write bridges?
  advantages - work with native predicates - transitivity etc for free

  NAME: ograph, ontograph
  
**/