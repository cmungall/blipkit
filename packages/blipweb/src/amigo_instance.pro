/* -*- Mode: Prolog -*- */
/** @copyright
  
  Copyright (C) 2005 by Chris Mungall (cjm AT fruitfly DOT org)
  
  @/copyright

  amigo component for querying/displaying data of class 'instance'
  
  */

% ==================== CONFIG ====================

%amigo_component(instance).
data_class(instance).
data_class_by_ont(_Ont,instance).
data_class_window_size(instance,20).

http_param_label(instance,'instances').

% ==================== MODEL ====================

/**
  @pred is_instance(?InstID) sd
*/
is_instance(ID):-
        inst_of(ID,_).



% delegate
amigo_query(instance,Q,IDL):-
        linstance_query(Q,IDL).


/**
  @pred instance_query(+SearchTerm,?ID) nd
  @pred linstance_query(+SearchTerm,?IDList) d
   search term can be:
   search(+Text) : text search
   class(+ID) : transitive instance-of
  */
linstance_query(Q,IDL):-
        xsetof(ID,instance_query(Q,ID),IDL).
instance_query(search(ID),ID):-
        is_instance(ID).
instance_query(search(Text),ID):-
        inst(ID,N),
        is_instance(ID),
        sub_atom(N,_,_,_,Text).
instance_query(search(Text),ID):-
        inst_sv(ID,_,V),
        is_instance(ID),
        sub_atom(V,_,_,_,Text).
instance_query(class(ClassID),InstID):-
        inst_ofRT(InstID,ClassID).

id_filter_set(instance,ID,SetID,SetN,type):-
        inst_of(ID,ClassID),
        belongs(ClassID,SetID),
        ontology(SetID,SetN).

% ==================== VIEW ====================

% slightly different from default...
inst:attribute_value_text(A,V) =>
 doc:'shows A=V',
 tr(class=attribute,
    td(href_id_as_label(property,A)),
    td(data(V))).

inst_rel(A,V) =>
 doc:'shows a relation to an object instance',
 tr(class=attribute,
    td(href_id_as_label(property,A)),
    td(href_class_or_instance(V))).

inst_rel(Su,A,Ob) =>
 doc:'shows a relation as infix between subject and object instance',
 tr(class=attribute,
    td(href_class_or_instance(Su)),
    td(href_id_as_label(property,A)),
    td(href_class_or_instance(Ob))).

href_class_or_instance(ID) =>
 if(class(ID,_),
    then:href_id_as_label(term,ID),
    else:href_id_as_label(instance,ID)).
       
% INSTANCE RESULTS

% PREDICATE
search_result_headers(instance,summary,
                      tr(th([]),th('ID'))).
                     
search_result_id_view(instance,summary,ID) =>
 if(inst_of(ID,Class),
    [td(href_id_as_label(instance,ID)),
     td(href_id_as_label(term,Class)),
     td(table(findall(inst:attribute_value_text(Slot,Val),
                      inst_sv(ID,Slot,Val,_))),
        table(findall(inst_rel(Slot,Val),
                      inst_rel(ID,Slot,Val))))]).

search_result_id_view(instance,detail,ID) =>
 call(inst_of(ID,C) leftjoin inst(ID,N)),
 td(class=instance_info,
    html:h2([C,
             ': ',
             N]),
    html:ul(tagval('ID',href_data_item(instance,ID,ID)),
            tagval('Label',N),
            tagval('Class',href_id_as_label(term,C))),
    html:br,
    table(findall(inst:attribute_value_text(Slot,Val),
                  inst_sv(ID,Slot,Val,_))),
    table(findall(inst_rel(Slot,Val),
                  inst_rel(ID,Slot,Val))),
    html:br,
    html:h3('Reciprocal relations'),
    table(findall(inst_rel(SID,Slot),
                  inst_rel(SID,Slot,ID))),
    html:br).

                                % -- END OF INSTANCE RESULTS

href_id_as_label(instance,ID) =>
 doc:'get label for instance and make href',
 if(inst(ID,N),
    then: href_data_item(instance,ID,N),
    else: href_data_item(instance,ID,ID)).

detail_page(instance) =>
 doc:'TOP: details on one (or more?) instances',
 outer('OBD - Instance search results',
       html:div(class=main,
                basic_query_form,
                report_by_data_class(instance))).                 

% ==================== CONTROLLER ====================

