/* -*- Mode: Prolog -*- */


:- module(ontol_bridge_from_owl2,
          [
           owlrestriction_to_oborelationship/2,
           uri_oboid/2
          ]).

% SEE ALSO: ontol_bridge_from_owl2_ext - in progress
% REMEMBER: -f thea2_owl on command line

:- use_module(bio(ontol_db),[]).
%:- use_module(bio(rdf_id_util),[rdfid_oboid/2]).
:- use_module(bio(mode)).
:- use_module(bio(bioprolog_util),[solutions/3]).
:- use_module(library('thea2/owl2_model')).
:- use_module(library('semweb/rdf_db'),[rdf_global_id/2]).

:- dynamic implicit_metarelation/3.

:- multifile suppress_entity/1.
% OE1 has issues with relationships to xsd types
suppress_entity(X) :- nonvar(X),atom(X),sub_atom(X,0,_,_,'http://www.w3.org/2001/XMLSchema#').
suppress_entity('http://www.w3.org/2002/07/owl#topObjectProperty').


% cut-n-pasted:
literal_value_type(literal(lang(en,X)),X,string):- !.
literal_value_type(literal(lang(_,X)),X,string):- !.
literal_value_type(literal(type(T1,X)),X,T):- !, convert_xsd_type(T1,T).
literal_value_type(literal(X),X,string):- !.

% shorten xsd url to prefix
convert_xsd_type(In,Out):-
        rdf_global_id(Prefix:Local,In),
        concat_atom([Prefix,Local],':',Out),
        !.
convert_xsd_type(X,X).

% ----------------------------------------
% HOOKS
% ----------------------------------------
:- multifile consumed_property/1.
consumed_property('rdfs:label').
% other modules may consume more

% ----------------------------------------
% DECLARATIONS
% ----------------------------------------
ontol_db:class(X) :-    uri_oboid(U,X),owl2_model:class(U),\+suppress_entity(U).
ontol_db:property(X) :- uri_oboid(U,X),owl2_model:objectProperty(U),\+suppress_entity(U).
%ontol_db:inst(X) :- uri_oboid(U,X),owl2_model:namedIndividual(U),\+suppress_entity(U).
ontol_db:inst(X) :- uri_oboid(U,X),owl2_model:classAssertion(_,U),\+suppress_entity(U).

% TODO: annotationAsserted --> is_obsolete

% ----------------------------------------
% ANNOTATIONS
% ----------------------------------------
% these are intended to be extended in specific modules
metadata_db:entity_label(X,Label) :- uri_oboid(U,X),owl2_model:labelAnnotation_value(U,Label).

metadata_db:idspace_uri(Local,Global):-
        used_idspace(Local),
        rdf_db:ns(Local,Global).

used_idspace(Local) :-
        setof(Local,
              ID^Res^(metadata_db:entity_resource(ID,Res),
                      metadata_db:id_idspace(ID,Local)),
              Locals),
        member(Local,Locals).

% some ontologies may have their own ways of indicating this
metadata_db:entity_obsolete(ID,class):-
        uri_oboid(Res,ID),
        annotationAssertion('owl:deprecated',Res,true).


% ----------------------------------------
% ONTOLOGY AXIOMS
% ----------------------------------------
ontol_db:ontology(Ont) :-
        % we want to ensure that this is set once, in the root
        % of the import chain
        \+ nb_current(owl_ontology,_),
        !,
        owl2_model:ontology(Ont), % if multiple onts loaded, this will be arbitrary
        \+ ontologyImport(_,Ont), % root of chain
        nb_setval(owl_ontology,Ont).
ontol_db:ontology(Ont) :-
        nb_current(owl_ontology,Ont).

ontol_db:import_directive(URI):-
        ontologyImport(_,URI).


% ----------------------------------------
% RELATION AXIOMS
% ----------------------------------------
ontol_db:is_transitive(X) :- uri_oboid(U,X),transitiveProperty(U).
ontol_db:is_reflexive(X) :- uri_oboid(U,X),reflexiveProperty(U).
ontol_db:is_irreflexive(X) :- uri_oboid(U,X),irreflexiveProperty(U).
ontol_db:is_symmetric(X) :- uri_oboid(U,X),symmetricProperty(U).
ontol_db:is_asymmetric(X) :- uri_oboid(U,X),asymmetricProperty(U).
ontol_db:is_functional(X) :- uri_oboid(U,X),functionalProperty(U).
ontol_db:is_inverse_functional(X) :- uri_oboid(U,X),inverseFunctionalProperty(U).
ontol_db:inverse_of(X,Y) :- uri_oboid(UX,X),uri_oboid(UY,Y),owl2_model:inverseProperties(UX,UY).

% NOTE: revisit in future versions: overloading obo subsumption relation for legacy reasons
ontol_db:subclass(X,Y) :-
        uri_oboid(UX,X),uri_oboid(UY,Y),owl2_model:subPropertyOf(UX,UY),
        objectProperty(UX),objectProperty(UY),
	\+ suppress_entity(UY).


% TODO: more complex role chains
ontol_db:holds_over_chain(X,Y,Z) :- uri_oboid(UX,X),uri_oboid(UY,Y),uri_oboid(UZ,Z),owl2_model:subPropertyOf(UX,propertyChain([UY,UZ])).

% all-some/all-only
ontol_db:property_relationship(R1,MR,R2) :- implicit_metarelation(R1,MR,R2).

ontol_db:property_domain(R,X) :- uri_oboid(UR,R),uri_oboid(UX,X),owl2_model:propertyDomain(UR,UX).
ontol_db:property_range(R,X) :- uri_oboid(UR,R),uri_oboid(UX,X),owl2_model:propertyRange(UR,UX).


% ----------------------------------------
% CLASS AXIOMS
% ----------------------------------------
%ontol_db:subclass(X,Y) :- uri_oboid(UX,X),uri_oboid(UY,Y),owl2_model:subClassOf(UX,UY),class(UX),class(UY),\+suppress_entity(UY).
ontol_db:subclass(X,Y) :- uri_oboid(UX,X),uri_oboid(UY,Y),owl2_model:subClassOf(UX,UY),referenceable(UX),referenceable(UY),classExpression(UX),classExpression(UY),\+suppress_entity(UY).
ontol_db:restriction(X,R,Y) :- uri_oboid(UX,X),owl2_model:subClassOf(UX,Restr),owlrestriction_to_oborelationship(Restr,R=Y).
ontol_db:min_cardinality_restriction(X,R,Card,Y) :- uri_oboid(UX,X),owl2_model:subClassOf(UX,Restr),owlrestriction_to_oborelationship(Restr,c(min,Card,R,Y)).
ontol_db:max_cardinality_restriction(X,R,Card,Y) :- uri_oboid(UX,X),owl2_model:subClassOf(UX,Restr),owlrestriction_to_oborelationship(Restr,c(max,Card,R,Y)).
ontol_db:exact_cardinality_restriction(X,R,Card,Y) :- uri_oboid(UX,X),owl2_model:subClassOf(UX,Restr),owlrestriction_to_oborelationship(Restr,c(exact,Card,R,Y)).

% so far we only allow intersection-constructs in genus-differentia form
ontol_db:genus(X,Y) :- class_genus_differentia(X,Y,_).
%ontol_db:genus(X,Y) :- setof(X-Y,Z^class_genus_differentia(X,Y,Z),XYs),member(X-Y,XYs).
ontol_db:differentium(X,R,Y) :- class_genus_differentia(X,_,DL),member(R=Y,DL).

% so far we only allow named classes in the union element
ontol_db:class_union_element(X,Y) :-
        uri_oboid(UX,X),uri_oboid(UY,Y),
        owl2_model:equivalentClasses(EL),
        member(UX,EL),class(UX),
        member(unionOf(UL),EL),
        member(UY,UL).

ontol_db:disjoint_from(X,Y) :- uri_oboid(UX,X),uri_oboid(UY,Y),owl2_model:disjoint_with(UX,UY).

% ----------------------------------------
% INSTANCE AXIOMS
% ----------------------------------------
ontol_db:inst_of(X,Y) :- uri_oboid(UX,X),uri_oboid(UY,Y),owl2_model:classAssertion(UY,UX).
ontol_db:inst_rel(X,R,Y) :- uri_oboid(UX,X),uri_oboid(UR,R),uri_oboid(UY,Y),owl2_model:objectPropertyAssertion(UR,UX,UY).
ontol_db:inst_sv(X,R,Y,DT) :- uri_oboid(UX,X),uri_oboid(UR,R),
        owl2_model:propertyAssertion(UR,UX,Lit),literal_value_type(Lit,Y,DT),
        \+ consumed_property(R),
        \+ consumed_property(UR).

%        owl2_model:dataPropertyAssertion(UR,UX,Lit),literal_value_type(Lit,Y,DT).

% ----------------------------------------
% Translation of expressions
% ----------------------------------------

% so far we only allow intersection-constructs in genus-differentia form
class_genus_differentia(C,G,DL) :-
        uri_oboid(UC,C),
        uri_oboid(UG,G),
        owl2_model:equivalent_to(UC,intersectionOf(UIL)),
        class(UC),
        select(UG,UIL,UDL),
        class(UG), % named genus only
        maplist(owlrestriction_to_oborelationship,UDL,DL).

%% owlrestriction_to_oborelationship(+Restr,?OboExpr)
owlrestriction_to_oborelationship(someValuesFrom(UR,UY),R_t=Y) :-
        uri_oboid(UR,R_i),
        uri_oboid(UY,Y),
        referenceable(UR),
        referenceable(UY),
        force_all_some(R_i,R_t).
owlrestriction_to_oborelationship(allValuesFrom(UR,UY),R_t=Y) :-
        uri_oboid(UR,R_i),
        uri_oboid(UY,Y),
        referenceable(UR),
        referenceable(UY),
        force_all_only(R_i,R_t).
owlrestriction_to_oborelationship(minCardinality(Card,UR,UY),c(min,Card,R_i,Y)) :-
        uri_oboid(UR,R_i),
        uri_oboid(UY,Y),
        referenceable(UR),
        referenceable(UY).
owlrestriction_to_oborelationship(maxCardinality(Card,UR,UY),c(max,Card,R_i,Y)) :-
        uri_oboid(UR,R_i),
        uri_oboid(UY,Y),
        referenceable(UR),
        referenceable(UY).
owlrestriction_to_oborelationship(exactCardinality(Card,UR,UY),c(exact,Card,R_i,Y)) :-
        uri_oboid(UR,R_i),
        uri_oboid(UY,Y),
        referenceable(UR),
        referenceable(UY).

owlrestriction_to_oborelationship(exactCardinality(Card,UR),RV) :-
        owlrestriction_to_oborelationship(exactCardinality(Card,UR,'bfo:Entity'),RV).

% TODO: type/instance level relation conversion
force_all_some(R,R).
force_all_only(R_i,R_t) :-
        fail,                   % TODO: make configurable
        atom_concat(R_t,'_only',R_i),
        assert_implicit_metarelation(R_t,all_only,R_i).

% TODO: allow non-named classes, auto-construct anonymous classes
:- multifile referenceable_hook/1.
referenceable(U) :-
        suppress_entity(U),
        !,
        fail.
referenceable(U) :-
        debug(owl2_ext,'?ref ~w',[U]),
        entity(U),
        !.
referenceable(U) :-
        referenceable_hook(U).



%
assert_implicit_metarelation(R_i,MR,R_t) :-
        implicit_metarelation(R_i,MR,R_t),
        !.
assert_implicit_metarelation(R_i,MR,R_t) :-
        assert(implicit_metarelation(R_i,MR,R_t)),
        !.

% URIs

uri_oboid(RdfID,OboID):-
        % new IDs; e.g. obo:FLU_nnnnnnn
        nonvar(RdfID),
        rdf_global_id(obo:ID_With_Underscore,RdfID),
        concat_atom([NS|Toks],'_',ID_With_Underscore),
        concat_atom(Toks,'_',Local),
        concat_atom([NS,Local],':',OboID),
        !.
uri_oboid(RdfID,OboID):-
        nonvar(RdfID),
        rdf_global_id(NS:Local,RdfID),
        !,
        concat_atom([NS,Local],':',OboID).
uri_oboid(RdfID,OboID):-   
        nonvar(OboID),
        \+ atomic(OboID),           % tolerate skolem IDs
        !,
        rdf_bnode(RdfID).
uri_oboid(RdfID,OboID):-   
        nonvar(OboID),
        atomic(OboID),
        concat_atom([NS,Local],':',OboID),
        (   rdf_db:ns(NS,_)
        ->  rdf_global_id(NS:Local,RdfID)
        ;   concat_atom(['http://purl.obolibrary.org/obo/',NS,'_',Local],RdfID)),
        !.
uri_oboid(RdfID,OboID):-
        var(RdfID),
        var(OboID),
        freeze(RdfID,uri_oboid(RdfID,OboID)),
        !.
uri_oboid(X,X):- !.



/** <module>  maps to OBO-style ontol_db model from OWL2 using Thea

  ---+ Synopsis

  ==
  :- use_module(bio(io),[load_biofile/2]).
  :- use_module(bio(ontol_db)).
  :- use_module(bio(ontol_bridge_from_owl2)).
  
  % access biopax OWL model using ontol_db predicates
  demo:-
    load_biofile(owl,'biopax-level1.owl'),
    class(ID,rna),
    format('In biopax, rna is a subclass of the following:~n',[]),
    forall(subclass(ID,PID),
           showclass(PID)).

  showclass(ID):-
    class(ID,N),
    format('Class ID:~w~nClass name:~w~n',[ID,N]).
  ==

  Command line usage:
  
  ==
  blip -i http://purl.obolibrary.org/obo/obi.owl -f thea2_owl io-convert -to ontol_db:pro
  ==

  
  ---+ Description

  * owl2_model:subClassOf/2 mapped to ontol_db:subclass/2 (for named classes)
  * owl2_model:subClassOf/2 mapped to ontol_db:restriction/3 (when 2nd argument is objectSomeValuesFrom/1)
  

  ---+ See Also

  * ontol_bridge_to_owl2.pro -- for the reverse transformation -- TODO

  
*/
