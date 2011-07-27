
:- module(curation_bridge_to_owl2,
	  [
           materialize_curations_as_owl/0
	   ]).

:- use_module(curation_db).
:- use_module(bio(ontol_db)).
:- use_module(bio(seqfeature_db)).

:- use_module(bio(ontol_bridge_to_owl2_and_iao)).
:- use_module(bio(ontol_bridge_to_owl2),[uri_oboid/2]).
:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).
:- use_module(library('thea2/owl2_plsyn')).
:- use_module(library('thea2/owl2_popl')).
:- use_module(library('thea2/owl2_util')).
:- use_module(library('thea2/owl2_model'),[]).


native_to_literal(Term,literal(Term)):- number(Term),!.
native_to_literal(Term,literal(lang(en,Term))).

% ----------------------------------------
% POPL
% ----------------------------------------


pre_op('http://purl.obolibrary.org/obo/occurs_in',occurs_in). % FOR TESTING!!
pre_op('http://purl.obolibrary.org/obo/GO_has',has).
pre_ap('http://purl.obolibrary.org/obo/IAO_describes',describes).
pre_ap('http://purl.obolibrary.org/obo/IAO_has_evidence',has_evidence).
pre_c( 'http://purl.obolibrary.org/obo/IAO_go_annotation','go annotation').



popl_rules([add propertyAssertion(R) where curation_db:curation_subject_property_value(_,_,R,_),
            add [classAssertion( 'describes' some (Gene and has some GO_Class),
                                 Ann),
                 namedIndividual(Ann),
                 annotationAssertion('http://www.w3.org/2000/01/rdf-schema#label',Ann,literal(Desc)),
                 classAssertion( 'go annotation', Ann)]
           where curation_bridge_to_owl2:curation_statement_summary(Ann,Gene,_,GO_Class,Desc),
            
            add annotationAssertion( 'has_evidence', Ann, literal(Ev)) where curation_db:curation_evidence_code(Ann,Ev),

            add [class(Gene),
                 annotationAssertion( 'http://www.w3.org/2000/01/rdf-schema#label', Gene, literal(Label))]
           where seqfeature_db:feature(Gene,Label)
            ]).

pre_declaration(annotationProperty(X)) :- pre_ap(X,_).
pre_declaration(objectProperty(X)) :- pre_op(X,_).
pre_declaration(class(X)) :- pre_c(X,_).
pre_declaration(annotationAssertion('http://www.w3.org/2000/01/rdf-schema#label',X,literal(V))) :- pre_ap(X,V).
pre_declaration(annotationAssertion('http://www.w3.org/2000/01/rdf-schema#label',X,literal(V))) :- pre_op(X,V).
pre_declaration(annotationAssertion('http://www.w3.org/2000/01/rdf-schema#label',X,literal(V))) :- pre_c(X,V).


materialize_curations_as_owl :-
        forall( pre_declaration(A),
                owl2_model:assert_axiom(A)),
        popl_rules(Rules),
        forall( member(Rule,Rules),
                popl_translate(Rule,[syntax(plsyn),translate(labels),post_translate(oboid_uri)])).

curation_statement_summary(Ann,Gene,Rel,Class,Desc) :-
        curation_statement_x(Ann,Gene,Rel,Class),
        entity_label(Gene,GeneLabel),
        phrase(owlx(Class),ClassToks),
        concat_atom(ClassToks,' ',ClassLabel),
        atomic_list_concat(['Annotation of ',GeneLabel,' to ',ClassLabel],Desc).

curation_statement_x(Ann,Gene,Rel,Class) :-
        curation_statement(Ann,Gene,Rel,Class),
        \+  curation_subject_property_value(Ann,Class,_,_).

curation_statement_x(Ann,Gene,Rel,Class and (CRel2 some Val)) :-
        curation_statement(Ann,Gene,Rel,Class),
        curation_subject_property_value(Ann,Class,CRel,Val),
        maprel(CRel,CRel2).


owlx(X) --> {atom(X),entity_label(X,N)},!,[N].
owlx(some(R,Y)) --> !,owlx(R),[' some '],owlx(Y).
owlx(A and B) --> !,owlx(A),[' and '],owlx(B).
owlx(X) --> {atom(X)},!,[X].
owlx(X) --> {sformat(A,'~q',[X])},[A].

maprel(occurs_in,'http://purl.obolibrary.org/obo/occurs_in') :- !.
maprel(X,X).







/*

  testing:

  blip -r go -ff go_assoc^packages/curation/t/data/gene_assoc.fb.tbl mireot-by-annotations > packages/curation/t/data/go_subset.obo
  blip -i packages/curation/t/data/go_subset.obo -ff go_assoc^packages/curation/t/data/gene_assoc.fb.tbl -u curation_bridge_to_owl2 -goal materialize_curations_as_owl io-convert -to owl2 -o ~/tmp/gotest.owl

  then try query:

  all observations of protein transport:

  ==
  describes some (has some 'protein transport')
  ==

  
*/
