/* -*- Mode: Prolog -*- */
/** @copyright
  
  Copyright (C) 2005 by Chris Mungall (cjm AT fruitfly DOT org)
  
  @/copyright

  amigo extension for cross-product data

  deprecate this? CHECK IF USED??
  
  */

% ==================== CONFIG ====================

amigo_component(xp).
option_term_view(xp).

class_name(ID,N):-
        class(ID,N).
class_name(ID,N):-
        var(N),
        not(class(ID,_)),
        N=ID.

% DEP?
xpparent_by_diff(ID,PID):-
        differentium(ID,_T,ToID),
        xpparents(ToID,ToPIDs),
        member(PID,ToPIDs).
xpparents(ID,PIDs):-
        (genus(ID,GenusID)
        ->  true
        ;   GenusID=ID),
        xsetof(PID,parentRT(GenusID,PID),PIDs1),
        xsetof(PID,xpparent_by_diff(ID,PID),PIDs2),
        append(PIDs1,PIDs2,PIDs).
non_xpparent(ID,PID):-
        xpparents(ID,PIDs2),
        parentRT(ID,PID),
        not(member(PID,PIDs2)).

% bit of a hack...
is_qualifier(ID):-  lexical_category(ID,adj).
is_qualifier(ID):-  belongs(ID,general).
is_qualifier(ID):-  belongs(ID,spatial).


% VIEW

search_result_id_view(term,xp,ID) =>
 doc:'cross-product untangled term view',
 html:div(class=tree,
          getparam(term_id,ID),
          xp_onto_tree(ID,'Generic term',null)).

non_xpparents_box(ID) =>
 html:div(class=foo,
          h3('other parent relationships'),
          ul(findall(tagval(rel,href_id_as_label(term,PID)),
                     non_xpparent(ID,PID)))).

xp_onto_tree(ID,ParentR,PID) =>
 doc:'untangled view of an ontology term',
 if((genus(ID,GenusID),setof(R:To,differentium(ID,R,To),DiffL)),
    then:[],
    else:call((GenusID=ID,DiffL=[]))),
 html:table(class=classdef,
            border=0,
            html:tr(html:th(colspan=2,
                            if(PID=null,
                               then:[], %[href_id_as_label(term,ID),ParentR,href_id_as_label(term,GenusID)],
                               else:[ParentR,href_id_as_label(term,ID)]))),
            if(is_qualifier(ID),
               then:html:td(html:i(' (qualifier)')),
               else:
              html:tr(html:td('---Generic term: ',href_id_as_label(term,GenusID),
                              xp:dagview(GenusID)),
                      html:td(if(DiffL=[],
                                 then:[],
                                 else:[href_id_as_label(term,ID),
                                       ', characteristics']),
                              map(R:To,xp_onto_tree(To,R,ID),DiffL))))).

xp:dagview(ID) =>
 doc:'non-expandable dag view',
 html:div(class=dag,
          call(xsetof(PID,parentRT(ID,PID),PIDs)),
          findall(xp:dagview_node(root,RootID,PIDs),
                  (   member(RootID,PIDs),
                      noparent(RootID)))).

xp:dagview_node(T,PID,AllIDs) =>
 doc:'node in a dagview',
 html:ul(html:li(xp:dag_rel_term(T,PID),
                 findall(xp:dagview_node(SubR,CID,AllIDs),
                         (   parent(CID,SubR,PID),
                             member(CID,AllIDs))))).
                     
xp:dag_rel_term(T,ID) =>
 doc:'show term with parent rel type',
 session(S),
 call(relname(T,TN)),
 font(size='-2',data(TN)),
 call((class_name(ID,N),
       ngetparam(S,highlighted_ids,IDhL),
       (member(ID,IDhL) -> Hi=1 ; Hi=0))),
 href_data_item(term,ID,[ID,':',N],Hi),
 use_term_button(ID).
