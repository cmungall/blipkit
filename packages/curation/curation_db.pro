:- module(curation_db,
          [
           curation/1,
           curation_statement/3,
           curation_statement/4,
           curation_statementT/4,
           curation_statementT/5,
	   curation_statementTI/4,
	   curation_statementTI/5,
           curation_statementT_over/6,
	   curation_statement_nr/4,
           curation_lca/4,
	   curation_subject_property_value/4,
           curation_isoform/2,
           negative_curation_statement/4,
           curation_qualifier/3,
           curation_evidence/2,
           evidence_type/2,
           evidence_with/2,
           curation_evidence_code/2,
           curation_evidence_with/2,
	   curation_statement_evidence_type/5,
	   curation_statement_evidence_type/4,
           curation_publisher/2,
           curation_source/2,           
           all_curation_creator/1,
           class_annotated_entity_count/2,
           annotation_congruence_for_subject/3,
           annotation_minimal_summary_for_subject/2,
           annotation_minimal_summary_by_predicate/3,
           lookup_feature/2,
           lookup_features/3,
           featureset_class_enrichment/2,
	   class_prob/2,
	   class_infocontent/2,
           class_coannotated_with/2,
           class_coannotated_with/3,
           class_not_coannotated_with/3,
           class_coannotated_with_summary/3,
           class_coannotated_with_summary/4,
           class_conditional_prob/3,
           class_conditional_prob/5
          ]).

:- use_module(bio(metadata_db)).
:- use_module(bio(ontol_db)).
:- use_module(bio(seqfeature_db)).
:- use_module(bio(bioprolog_util)).
:- use_module(bio(stats_distributions)).

:- use_module(bio(dbmeta)).

%% curation(?Curation)
% true if Curation denotes a curated statement. Basic unit of work
:- extensional(curation/1).

%% curation_statement(?Curation,?Subject,?Relation,?Object)
%  each curation can have zero to many statements
%  (a curation is typically a single statement)
:- extensional(curation_statement/4).

%% curation_statement(C,S,Ob)
% as curation_statement/4
curation_statement(C,S,Ob) :-
	curation_statement(C,S,has_role,Ob).


%% negative_curation_statement(?Curation,?Subject,?Relation,?Object)
:- extensional(negative_curation_statement/4).

%% curation_qualifier(?Curation,?Type,?QualifierValue)
% col 4 in GAF
:- extensional(curation_qualifier/3).

%% curation_subject_property_value(?Curation,?SubjectClass,?Property,?Value)
% col16 in GAF
:- extensional(curation_subject_property_value/4).

:- extensional(curation_isoform/2).


%% curation_evidence(?Curation,?Evidence)
:- extensional(curation_evidence/2).

%% evidence_type(?Evidence,?Type)
:- extensional(evidence_type/2).

%% evidence_with(?Evidence,?With)
:- extensional(evidence_with/2).

curation_evidence_code(A,Code):-
        curation_evidence(A,E),
        evidence_type(E,Code).
curation_evidence_with(A,With):-
        curation_evidence(A,E),
        evidence_with(E,With).


%% curation_statement_evidence_type(C,S,R,Ob,ET)
curation_statement_evidence_type(C,S,R,Ob,ET) :-
	curation_statement(C,S,R,Ob),
	curation_evidence_code(C,ET).

%% curation_statement_evidence_type(C,S,Ob,ET)
curation_statement_evidence_type(C,S,Ob,ET) :-
	curation_statement(C,S,Ob),
	curation_evidence_code(C,ET).


%% curation_publisher(?Curation,?Publisher)
% combination of curation/1 and entity_publisher/2
% in GO annotation, the publisher is the source/assigned_by column
curation_publisher(C,P):-
        entity_publisher(C,P),
        curation(C).

%% curation_source(?Curation,?Source)
% combination of curation/1 and entity_source/2
% provenance of curation - eg PubMed identifier
curation_source(C,P):-
        entity_source(C,P),
        curation(C).

%% curation_statementT(?Curation,?Subject,?R,?Ob)
% as curation_statementT/5
curation_statementT(C,S,R,Ob):-
        curation_statementT(C,S,R,Ob,_).

%% curation_statementT(?Curation,?Subject,?R,?Ob,?ObDirect)
% true if Subject is annotated to ObDirect, and Ob is in the reflexive transitive closure of ObDirect;
% i.e. Subject is indirectly annotated to Ob
curation_statementT(C,S,R,Ob,ObDirect):-
        % be defensive against Ob ground to node near root;
        % seq scan through annotations first..
        curation_statement(C,S,R,ObDirect),
        %debug(curation_db,'checking ~w',[curation_statement(C,S,R,ObDirect)]),
        parentRT(ObDirect,Ob).
%%subclassRT(ObDirect,Ob).

%curation_statementT(C,S,R,Ob,ObDirect):-
%	(   nonvar(S)
%        ->  curation_statement(C,S,R,ObDirect),
%	    parentRT(ObDirect,Ob)
%	;   parentRT(ObDirect,Ob),
%	    curation_statement(C,S,R,ObDirect)).


curation_statementTI(C,S,R,Ob):-	curation_statementTI(C,S,R,Ob,_).
curation_statementTI(C,S,R,Ob,ObDirect):-	curation_statementT(C,S,R,Ob,ObDirect).
curation_statementTI(C,S,R,Ob,ObDirect):-	curation_statementT(C,Ob,R,S,ObDirect).

	

%% curation_statement(?Curation,?Subject,?R,?Ob)
% as curation_statement/4, but exclude redundant annotations
% (redundancy checked via parentT/2)
curation_statement_nr(C,S,R,Ob):-
        curation_statement(C,S,R,Ob),
	\+ ((curation_statement(_,S,R,Ob2),
	     parentT(Ob2,Ob))).



curation_statementT_over(C,S,R,Ob,ObDirect,OverRel):-
        % be defensive against Ob ground to node near root;
        % seq scan through annotations first..
        curation_statement(C,S,R,ObDirect),
        debug(curation_db,'checking ~w',[curation_statement(C,S,R,ObDirect)]),
        parent_overT(OverRel,ObDirect,Ob).

curation_lca(S1,S2,R,C) :-
        curation_statement(_,S1,R,Ob1),
        curation_statement(_,S2,R,Ob2),
        parentRT(Ob1,C),
        parentRT(Ob2,C),
        \+ ((parentRT(Ob1,C2),
             parentRT(Ob2,C2),
             parentT(C2,C))).

all_curation_creator(L):-
        solutions(S,(curation_statement(A,_,_,_),entity_resource(A,S)),L).

class_annotated_entity_count(C,Num):-
	%aggregate(count(E),curation_statementT(A,E,R,C,AC),count(Num)).
	setof_count(E,curation_statementT(_A,E,_R,C,_AC),Num).
%        Num is count_distinct(E,(curation_statementT(_,E,_,C,_))).

annotation_congruence_for_subject(Su,TotalObs,Score):-
        solutions(Ob,(curation_statement(_,Su,_,Ob1),parentRT(Ob1,Ob)),AllObs),
        length(AllObs,TotalObs), % all colored nodes
        solutions(Src,(curation_statement(A,Su,_,_),entity_resource(A,Src)),Srcs),
        findall(Src-NumObs,
                (   member(Src,Srcs),
                    solutions(Ob,(curation_statement(A,Su,_,Ob1),entity_resource(A,Src),parentRT(Ob1,Ob)),Obs),
                    length(Obs,NumObs)),
                SrcNumPairs),
        length(SrcNumPairs,NumSrcs),
        (   NumSrcs>0
        ->  sumof(NumObs,member(_-NumObs,SrcNumPairs),TotalNumObs),
            AvgNumObs is TotalNumObs/NumSrcs,
            Score is AvgNumObs/TotalObs
        ;   Score=0 ).


annotation_congruence_by_source_pair(Su,S1,S2,NumObs):-
        findall(Ob,(curation_statement(A,Su,_,Ob1),(entity_resource(A,S1);entity_resource(A,S2)),parentRT(Ob1,Ob)),Obs),
        length(Obs,NumObs).

annotation_minimal_summary_for_subject(Su,MinimalObSrcs):-
        solutions(Ob-Src,(curation_statement(A,Su,_,Ob1),
                          parentRT(Ob1,Ob),
                          entity_resource(A,Src)
                         ),
                  ObSrcPairs),
        solutions(Ob,member(Ob-_,ObSrcPairs),Obs),
        solutions(Ob-Srcs,(member(Ob,Obs),
                           solutions(Src,member(Ob-Src,ObSrcPairs),Srcs)),
                  ObSrcs),
        % if there is a subclass (more specific annotation), filter it.
        % note that more specific classes include composed classes;
        findall(Ob-Srcs,(member(Ob-Srcs,ObSrcs),
                         \+ ((
                              member(ObSub-Srcs,ObSrcs),
                              parentT(ObSub,Ob)))),
                MinimalObSrcs).

% todo: DRY
% MinimalObSrcs = [ Object-[Src1,Src2,...], ...]
annotation_minimal_summary_by_predicate(Su,Pred,MinimalObSrcs):-
        solutions(Ob-Src,(Pred,
                          curation_statement(A,Su,_,Ob1),
                          parentRT(Ob1,Ob),
                          entity_resource(A,Src)
                         ),
                  ObSrcPairs),
        solutions(Ob,member(Ob-_,ObSrcPairs),Obs),
        solutions(Ob-Srcs,(member(Ob,Obs),
                           solutions(Src,member(Ob-Src,ObSrcPairs),Srcs)),
                  ObSrcs),
        % if there is a subclass (more specific annotation), filter it
        findall(Ob-Srcs,(member(Ob-Srcs,ObSrcs),
                         \+ ((
                              member(ObSub-Srcs,ObSrcs),
                              parentT(ObSub,Ob)))),
                MinimalObSrcs).

%% class_coannotated_with(+ClassA,?ClassB) is nondet
class_coannotated_with(A,B) :-
        curation_statementT(_,G,has_role,A,_),
        curation_statementT(_,G,has_role,B,_).

class_coannotated_with(A,B,G) :-
        curation_statementT(_,G,has_role,A,_),
        curation_statementT(_,G,has_role,B,_).

class_not_coannotated_with(A,B,G) :-
        curation_statementT(_,G,has_role,A,_),
        \+ curation_statementT(_,G,has_role,B,_).

class_coannotated_with_summary(A,B,Num) :-
        class(A),
        class(B),
        Num is count_distinct(G,(curation_statementT(_,G,has_role,A,_),
                                 curation_statementT(_,G,has_role,B,_))).
class_coannotated_with_summary(A,B,G,Num) :-
        curation_statementT(_,G,has_role,A,_),
        class(B),
        Num is count_distinct(G,curation_statementT(_,G,has_role,B,_)).

%% class_conditional_prob(A,B,ProbAGivenB)
% ProbAGivenB = p(A|B)
% A and B are classes. See class_conditional_prob/5
class_conditional_prob(A,B,ProbAGivenB):-
        class_conditional_prob(A,B,_,_,ProbAGivenB).

%% class_conditional_prob(A,B,CountAandB,CountB,ProbAGivenB)
% ProbAGivenB = p(A|B)
% A and B are classes.
% here p(A) is the probability of a gene being annotated to A,
% and p(A|B) is the probability of a gene being annotated to A given that it is annotated to B
class_conditional_prob(A,B,CountAandB,CountB,ProbAGivenB):-
        class(A),
        class(B),
        CountAandB is count_distinct(G,(
                                        curation_statementT(_,G,has_role,B,_),
                                        curation_statementT(_,G,has_role,A,_))),
        CountB is count_distinct(G,(
                                    curation_statementT(_,G,has_role,B,_))),
        ProbAGivenB is CountAandB/CountB.


        

%% class_prob(?Class,?Prob)
class_prob(C,P):-
	NumC is count_distinct(G,curation_statementT(_,G,_,C,_)),
	NumAll is count_distinct(G,curation_statementT(_,G,_,_,_)),
	P is NumC/NumAll.

%% class_infocontent(?Class,?InformationContent)
% IC = -log2(p(Class))
class_infocontent(C,IC):-
	class_prob(C,P),
	IC is -log(P)/log(2).

lookup_features(IDs,Features,NotFoundL) :-
        findall(ID-F,(member(ID,IDs),
                   lookup_feature(ID,F)),
                Map),
        findall(F,member(_-F,Map),Features),
        findall(ID,(member(ID,IDs),\+member(ID-_,Map)),NotFoundL).

lookup_feature(F,F) :-
        feature(F),
        !.
lookup_feature(ID,F) :-
        entity_label(F,ID),
        feature(F),
        !.
lookup_feature(ID,F) :-
        entity_synonym(F,ID),
        feature(F),
        !.
lookup_feature(ID,F) :-
        feature(F),
        concat_atom([_,ID],':',F),
        !.

xxxclass_used_twice(Class,FClassPairs) :-
        \+ \+ ((member(F1-Class,FClassPairs),
                member(F2-Class,FClassPairs),
                F1\=F2)).

class_used_twice(Class) :-
        \+ \+ ((curation_statementT(_,F1,_,Class), % expect this to be tabled..
                curation_statementT(_,F2,_,Class), % expect this to be tabled..
                F1\=F2)).

        

%% featureset_class_enrichment(+FSampleMembers:list,?ClassStats:list)
% FSampleMembers - each member must match a seqfeature_db:feature/1 
% tabling recommended - curation_statementT/4 and parentT/2
%
% TODO: call R here
featureset_class_enrichment(FSampleMembers,ClassStats):-
        length(FSampleMembers,FSampleSize),
	setof_count(F,curation_statement(_,F,_,_),DBSize),
        debug(enrichment,'DBSize=~w',[DBSize]),
	% get all annotations for everything in the feature set
        %findall(F-Class,(member(F,FSampleMembers),
	%		 curation_statementT(_,F,_,Class)),
        %        FClassPairs),
        solutions(Class,(member(F,FSampleMembers),
			 curation_statementT(_,F,_,Class)),
                  Classes),
        debug(enrichment,'Found all gene->class annotations',[]),
        % all Classes used
        %solutions(Class,member(_-Class,FClassPairs),Classes),
        debug(enrichment,'Found all classes used',[]),
	% bonferoni for classes with >1 annotation
        setof(Class,(member(Class,Classes),
                     class_used_twice(Class)),
              MClasses),
	length(MClasses,CorrectionFactor),
        debug(enrichment,'bonferoni correction factor (i.e. number of classes with >1 annotation)=~w',[CorrectionFactor]),
	findall(Class-Stats,
		(   member(Class,Classes),
		    annotations_class_probability(DBSize,CorrectionFactor,FSampleSize,Class,Stats)),
		ClassStats).

%annotations_class_probability(FClassPairs,DBSize,CorrectionFactor,FSampleSize,Class,Stats):-
annotations_class_probability(DBSize,CorrectionFactor,FSampleSize,Class,Stats):-
        debug(enrichment,'class: ~w',[Class]),
        %solutions(F,member(F-Class,FClassPairs),FSampleAnnotatedMembers),
        setof(F,A^Rel^curation_statementT(A,F,Rel,Class),FSampleAnnotatedMembers),
        length(FSampleAnnotatedMembers,FSampleAnnotatedSize),
        debug(enrichment,'  annotated sample size: ~w',[FSampleAnnotatedSize]),
        aggregate(F2,
                  A^Rel^curation_statementT(A,F2,Rel,Class), % expect this to be tabled..
                  DBAnnotatedSize),
        debug(enrichment,'  annotated in db size: ~w',[DBAnnotatedSize]),
        p_value_by_hypergeometric(FSampleAnnotatedSize,FSampleSize,DBAnnotatedSize,DBSize,PValue),
	CPValue is CorrectionFactor * PValue,
        Stats=[corrected_p_value(CPValue)].

% -------------------- TESTS --------------------
% regression tests to ensure behaviour of module is correct;
% lines below here are not required for module functionality

unittest(load(go)=
      load_bioresource(go)/[]).

unittest(load(assocs)=
      load_biofile(go_assoc,'gene_assoc.fb.tbl')/[]).

unittest(test(fly1,
            [_=load(assocs)],
            (   ensure_loaded(bio(ontol_db)),
		ensure_loaded(bio(ontol_writer_text)),
                ensure_loaded(bio(curation_db)),
                ensure_loaded(bio(bioprolog_util)),
		load_bioresource(go),
                forall(curation(C),
		       (   ontol_writer_text:show_curation_statement(C,[])))),
            true)).

/** <module> representation of curated statements

  ---+ Synopsis

==
:- use_module(bio(curation_db)).

% 
demo:-
  nl.
  

==

---+ Details

This module extends ontol_db.pro with curation statements (aka annotations).



@author  Chris Mungall
@version $Revision$
@see     README
@license License


*/
