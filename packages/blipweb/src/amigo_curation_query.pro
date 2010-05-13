/* -*- Mode: Prolog -*- */
/** @copyright
  
  Copyright (C) 2005 by Chris Mungall (cjm AT fruitfly DOT org)
  
  @/copyright

  amigo component for querying/displaying data of class 'mutant'

  two dataclasses: phenotype and mutant
  
  */

% ==================== CONFIG ====================

:- use_module(bio(curation_db)).
:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).
:- use_module(bio(dbmeta)).
:- use_module(bio(tabling)).

amigo_component(curation).

data_class(curation).
data_class_by_ont(_Ont,curation).

http_param_label(curation,curation).

% ==================== MODEL ====================

% delegate
amigo_query(curation,Q,IDL):-
        lcuration_query(Q,IDL).
lcuration_query(Q,IDL):-
        xsetof(ID,curation_query(Q,ID),IDL).
curation_query(search(Text),Cur):-
        entity_query(search(Text),AE),
        curation_statement(Cur,_,_,AE).

% filters - partitions data into sets that can be used by tabs
id_filter_set(curation,ID,Set,Set,ns):-
        entity_resource(ID,Set).

% ==================== VIEW ====================

% genotypes

href_id_as_label(curation,ID) =>
  doc:'show curation ID as label',
  if(entity_label(ID,N),
     then: href_data_item(curation,ID,noesc(N),0),
     else: html:i(href_data_item(curation,ID,ID))).

search_result_id_view(curation,summary,ID) =>
 doc:'Summary View of a single ID',
 if(curation_statement(ID,S,R,O),
    [
     td(href_label(S)),
     td(href_id_as_label(term,R)),
     td(href_id_as_label(term,O)),
     td(href_label(Source) where curation_source(ID,Source)),
     td(href_label(Publisher) where curation_publisher(ID,Publisher)),
     td(evidence(Ev) forall curation_evidence(ID,Ev))
    ]).

evidence(Ev) =>
 div(class=evidence,
     href_label(Type) forall evidence_type(Ev,Type),
     href_label(With) forall evidence_with(Ev,With)).

search_result_id_view(curation,detail,ID) =>
 doc:'Detail View of a single ID',
 td(div(class=info,
        if(curation_statement(ID,S,R,O),
           [
            td(href_label(S)),
            td(href_id_as_label(term,R)),
            td(href_id_as_label(term,O))
           ]))).

href_label(ID) =>
 doc:'Show label and hyperlink to detail page',
 if(entity_label(ID,Label),
    then:
   href_data_item(class,ID,Label),
    else:
   href_data_item(class,ID,ID)).


