/* -*- Mode: Prolog -*- */
/** @copyright
  
  Copyright (C) 2005 by Chris Mungall (cjm AT fruitfly DOT org)
  
  @/copyright

  amigo component for querying/displaying data of class 'feature'
  
  */

% ==================== CONFIG ====================

:- use_module(bio(seqfeature_db)).
:- use_module(bio(goa_db)).

amigo_component(feature).
data_class(feature).
data_class_by_ont(_Ont,feature).
data_class_window_size(feature,20).

http_param_label(feature,'feature data').

% ==================== MODEL ====================

/**
  @pred is_feature(?InstID) sd
   succeeds if InstID is a "feature"; eg genotype, SO instance
  NOT COMPLETE: hardcodes types
*/
is_feature(ID):-
        inst_of(ID,genotype).
is_feature(ID):-
        inst_of(ID,gene).

/**
  @pred feature_role(?FeatureInstID,?RoleInstID) nd
   relates a feature to its role; role can be pheno instance
   or an instance of eg a GO term.

  a pheno instance will have sub-instances for relates_to, attr, stage, etc
*/
feature_role(ID,IDr):-
        inst_sv(ID,T,IDr),
        role_attr(T).


/**
  @pred feature_genetext(+InstID,?Text) sd
   name of corresponding gene
*/
feature_genetext(ID,Text):-
        inst_of(ID,Type),
        Type\=genotype,
        inst(ID,Text).
feature_genetext(ID,Text):-
        inst_of(ID,genotype),
        inst_sv(GID,has_genotype,ID),
        inst(GID,Text).

/**
  @pred feature_data(+ID,?N,?IDOrg,?GeneText,?RoleInstIDList) nd
   gets hi-level feature view from underlying generic instances
  */
feature_data(ID,N,IDOrg,GText,InstL):-
        inst(ID,N),
        feature_organism(ID,IDOrg),
        feature_genetext(ID,GText),
        xsetof(Inst,feature_role(ID,Inst),InstL).

% delegate
amigo_query(feature,Q,IDL):-
        lfeature_query(Q,IDL).


/**
  @pred feature_query(+SearchTerm,?ID) nd
  @pred lfeature_query(+SearchTerm,?IDList) d
   search term can be:
   search(+Text) : text search
   class(+ID) : transitive instance-of
  */
lfeature_query(Q,IDL):-
        xsetof(ID,feature_query(Q,ID),IDL).
feature_query(search(Text),ID):-
        inst(ID,N),
        is_feature(ID),
        sub_atom(N,_,_,_,Text).
feature_query(search(ID),ID):-
        is_feature(ID).
% basic feature<->role [role is an instance from an ontology]
feature_query(class(ID),IDf):-
        inst_sv(IDf,has_role,Inst),
        instRTA(Inst,ID).
% anon class
feature_query(class(ID,RL),IDf):-
        inst_sv(IDf,has_role,Inst), % try all features
        instRTA(Inst,ID),
        forall(member(Slot:class(SVID),RL),
               (   inst_sv(Inst,Slot,ChildID),
                   parentRT(ChildID,SVID))).
% feature<->role<->entity (eg pato) [role is 'phenotype']
feature_query(class(ID),IDf):-
        inst_sv(IDf,Attr,RoleInst),
        role_attr(Attr),
        inst_sv(RoleInst,relates_to,TypedInst),
        instRTA(TypedInst,ID).
% feature<->role<->qualifier (eg xp) [role is eg GO term]
feature_query(class(ID),IDf):-
        inst_sv(IDf,Attr,RoleInst),
        role_attr(Attr),
        inst_sv(RoleInst,_,QualClassID),
        parentRT(QualClassID,ID).


id_filter_set(feature,ID,Set,Set,type):-
        inst_of(ID,Set).

% ==================== VIEW ====================

% FEATURE RESULTS

% PREDICATE
search_result_headers(feature,summary,
                      tr(th([]),th('ID'),th('Name'),th('Entity'),th('Organism'),th('Annotation'))).

search_result_id_view(feature,summary,ID) =>
 if(feature(ID,N,T),        
    [td(href_data_item(feature,ID,ID)),
     td(N),
     td(T),
     td(foo),
     td(if(feature_organism(ID,OrgID),
           then:OrgID,
           else:[])),
     td(findall(association_table(association(AID,CID,ID,Quals),_Inst),
                association(AID,CID,ID,Quals)))]).

search_result_id_view(feature,detail,ID) =>
 call((inst_of(ID,C),
       feature_data(ID,N,IDOrg,GText,InstL))),
 td(class=feature_info,
    h2([C,': ',N]),
    ul(tagval('ID',ID),
       tagval('Gene',GText),
       tagval('Organism',IDOrg)),
    hr,
    table(tr(th([]),th(' -- Annotation --')),
          tr(td([]),td(
                       map(Inst,instance_table(ID,Inst),InstL)
                      )))).
                                % -- END OF FEATURE RESULTS

association_table(association(_AID,_CID,_FID,_Quals)) =>
 doc:'generic table display for instance data. will show
 the instance class with attribute value data nested underneath',
 doc:example('gut
              at_stage Larval:Day4'),
 table(call((inst_of(ID,C),
             findall(A=V,generic_inst_sv(ID,A,V),AVL))),
       if(C=phenotype,
          then:[
                                % we should combine all in 1 TODO
                call((inst_sv(ID,relates_to,PhentID),
                      inst_of(PhentID,PhentClassID))),
                tr(td(colspan=3,href_id_as_label(term,PhentClassID))),
                genotype_phent_rows(_FeatureID,PhentID)
               ],
          else:
         tr(
            if(instance_evidence(ID,Evid),then:td(Evid),else:td([])),
            td(colspan=2,href_id_as_label(term,C)),
            map(A=V,
                tr(td([]),td([]),
                   td(font(size='-2',href_id_as_label(term,A)),
                      href_id_as_label(term,V))),
                AVL)))).

detail_page(feature) =>
 doc:'TOP: details on one (or more?) features',
 outer('OBD - Feature search results',
       html:div(class=main,
                basic_query_form,
                report_by_data_class(feature))).

% ==================== CONTROLLER ====================


