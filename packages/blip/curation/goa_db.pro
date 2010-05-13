/* -*- Mode: Prolog -*- */


:- module(goa_db,
          [
           % intensional predicates
           association/4,
           association_property_value/3,
           evidence/4,
           % extensional predicates
           association/3,
           association/6,
           association/7,
           associationRT/5,

           goa_distinct_feature_count/1,
           goa_distinct_feature_count_by_term/2,

           evidence_code_to_class/2,
           agent_role_by_class/2,

           featureset_term_stats/4
          ]).

:- use_module(bio(ontol_db)).
:- use_module(bio(seqfeature_db)).
:- use_module(bio(bioprolog_util)).
:- use_module(bio(stats_distributions)).

% metadata on the data predicates used in this module
:- use_module(bio(dbmeta)).
:- datapreds(goa_db,[
                    association(assocId,annotClassId,featureId,qualifiers),
                    association_property_value(assocId,type,to),
                    evidence(evidenceId,assocId,evClassId,refId)
                    ]).

% --

% VIEW PREDICATES

%% association(?ID,?ClassID,?FeatureID)
%   intensional view over association/4
%  
association(ID,ClassID,FeatureID):-
        association(ID,ClassID,FeatureID,0).

%% association(?AID,?ClassID,?FeatureID,?Q,?FeatureN,?FeatureClassID)
%
%  denormalized view over cartesian production of association/4
%and feature/3
%  
%  
association(AID,ClassID,FeatureID,Q,FeatureN,FeatureClassID):-
        association(AID,ClassID,FeatureID,Q),
        feature(FeatureID,FeatureN,FeatureClassID).
%% association(?AID,?ClassID,?FeatureID,?Q,?FeatureN,?FeatureClassID,?EvCode)
%
%  denormalized view over cartesian production of association/4
%and feature/3 and evidence/4
association(AID,ClassID,FeatureID,Q,FeatureN,FeatureClassID,Ev):-
        association(AID,ClassID,FeatureID,Q),
        feature(FeatureID,FeatureN,FeatureClassID),
        evidence(_,AID,Ev,_).
        

%% associationRT(?QueryClass,?ID,?ClassID,?FeatureID,?IsNot)
%   succeeds when FeatureID is assigned directly to ClassID or to
%  any child of QueryClassID
%
%  currently succeeds multiple times for duplicates for multiple paths
%  in DAG - TODO FIX
%  
associationRT(QueryClassID,ID,ClassID,FeatureID,Q):-
        association(ID,ClassID,FeatureID,Q),
        parentRT(ClassID,QueryClassID).
%        (   var(QueryClassID)
%        ->  association(ID,ClassID,FeatureID,Q),
%            parentRT(ClassID,QueryClassID)
%        ;   parentRT(ClassID,QueryClassID),
%            association(ID,ClassID,FeatureID,Q)).
            
goa_distinct_feature_count(Num):-
        setof_count(FID,association(_,_,FID,0),Num).
goa_distinct_feature_count_by_term(CID,Num):-
        class(CID,_),           % ensure ground
        setof_count(FID,associationRT(CID,_,_,FID,0),Num).

%
agent_role_by_class(Class,Relation):-
        agent_role_by_class1(Class,Relation),
        !.
agent_role_by_class(_,'oban:has_role').

agent_role_by_class1(Class,'OBO_REL:has_function'):- belongs(Class,molecular_function).
agent_role_by_class1(Class,'OBO_REL:participates_in'):- belongs(Class,biological_process).
agent_role_by_class1(Class,'OBO_REL:located_in'):- belongs(Class,cellular_component).

evidence_code_to_class(Code,Class):- synonym(Class,exact,Code),!.
evidence_code_to_class(Code,Code).

% (+,-,-)
featureset_term_stats(FSampleMembers,Class1,hypergeometric,Stats):-
        length(FSampleMembers,FSampleSize),
        goa_distinct_feature_count(DBSize),
        findall(F-Class,(member(F,FSampleMembers),
                         association(_,AnnotatedClass,F),
                         parentRT(AnnotatedClass,Class)),
                FClassPairs),
        solutions(Class,member(_-Class,FClassPairs),Classes),
        member(Class1,Classes),
        debug(stats,'class: ~w',[Class1]),
        solutions(F,member(F-Class1,FClassPairs),FSampleAnnotatedMembers),
        length(FSampleAnnotatedMembers,FSampleAnnotatedSize),
        debug(stats,'  annotated sample size: ~w',[FSampleAnnotatedSize]),
        setof_count(F2,(parentRT(AnnotatedClass,Class1),
                        association(_,AnnotatedClass,F2)),
                    DBAnnotatedSize),
        debug(stats,'  annotated in db size: ~w',[DBAnnotatedSize]),
        p_value_by_hypergeometric(FSampleAnnotatedSize,FSampleSize,DBAnnotatedSize,DBSize,PValue),
        Stats=stats(PValue).

% -------------------- TESTS --------------------
% regression tests to ensure behaviour of module is correct;
% lines below here are not required for module functionality

unittest(test(enrichment,
             [],
             (   ensure_loaded(bio(goa_db)),
                 ensure_loaded(bio(ontol_db)),
                 ensure_loaded(bio(tabling)),
                 load_bioresource(go),
                 load_biofile(go_assoc,'gene_assoc.fb.tbl'),
                 table_pred(parentT/3),
                 forall((featureset_term_stats([
                                                'FB:FBgn0027092',
                                                'FB:FBgn0002069'
                                               ],Class,hypergeometric,Stats)),
                        format('~w ~w~n',[Class,Stats]))),
            true)).





        
/** <module>   
  @author Chris Mungall
  @version  $Revision: 1.6 $
  @date  $Date: 2006/02/10 23:29:39 $
  @license LGPL

  ---+ Name
  ---++ goa_db
- gene ontology style association data

  ---+ Synopsis
  
  ==
  :- use_module(bio(io)).
  :- use_module(bio(goa_db)).
  :- use_module(bio(ontol_db)).

  demo:-
    load_biofile(go_assoc,'gene_assoc.fb.tbl'),
    load_bioresource(go),         % loads class/2, subclass/2 etc
    QN=membrane,
    class(QueryClassID,QN),        % fetch GO ID
    format('Querying "~w"~n',[QN]),
    forall(associationRT(QueryClassID,AssocID,ClassID,FeatureID,Qual),
           (   class(ClassID,ClassName),
               feature(ID,GeneName,_),
               format('  Gene: ~w [assigned directly to ~w ~w]~n',
                      [GeneName,ClassID,ClassName]),
               forall(evidence(AssocID,EvID),
                      format('    (evidence ~w)~n',[EvID])))).
  ==
  
  ---+ Description

  models GO-style associations; see

  <http://www.geneontology.org/GO.current.annotations.shtml> GO

  data can be imported - via biotable?
  
  ---+ Dependencies

  
  * seqfeature_db - refers to features (aka gene products)
  * ontol_db - refers to classes (aka terms)
  
  */