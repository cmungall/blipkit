/* ***********************************************************

   amigo_term

   viewing and querying ontologies

   pages:

   main : basic DAG view
   search_term_results_main   
   term_report

   this plugin is extendable with other plugins

   when showing a term in detail, instances of that term/class
   will also be shown. the function used depends on the ontology
   and which plugins are present
   
************************************************************/

% BioProlog
:- use_module(bio(serval)).
:- use_module(bio(ontol_db)).
:- use_module(bio(query_ontol)).
:- use_module(bio(bioprolog_util)).
:- use_module(bio(tabling)).
:- use_module(bio(graph)).

% AmiGO specific useful predicates
:- use_module(amigo_util).

% Extension
:- consult(amigo_term_lineage_widget).
:- consult(amigo_xp_matrix).

%:- table_pred(ontol_db:noparent/2).
:- table_pred(ontol_db:noparent_by_type/2).
                
%:- consult(amigo_property).

% ==================== CONFIG ====================

:- consult(amigo_core).

% serval params
form_method('GET').
init_page(main).

% amigo params
data_class(term).
% this is overridden in components
data_class_by_ont(_Ont,_DataClass):- fail.
data_class_window_size(term,100).

option_term_view(summary).
option_term_view('DAG').

hidden_param(term_filter_on).
hidden_param(show_relationship_type).

send_option(term,replace_tree).
send_option(term,plus_node).
http_param_label(replace_tree,'Make new tree').
http_param_label(plus_node,'Append to tree').

http_param_label(term,'ontologies').

http_param_label(redraw_dag,'Redraw DAG').

relationship_type_option(all).
relationship_type_option(subclass).
relationship_type_option(T):-
        is_transitive(T).
%        property(T,_).

relname(T,N):- relname1(T,N),!.
relname1(subclass,is_a).
relname1(T,N):- property(T,N).
relname1(T,T).

% hide_attr(?Attr)
%  these attributes have special meaning in
%  the hi-level view, and should be shown in appropriate place
hide_attr(name).
hide_attr(symbol).
hide_attr(syn).
hide_attr(in_organism).
hide_attr(has_phenotype).
hide_attr(relates_to).
hide_attr(with_evidence).

% ==================== MODEL ====================

%%%%%%%%%%%%%%%%%%%%
% -- Logic --
%%%%%%%%%%%%%%%%%%%%

check(parent_cycle(_ID,_P)).
check(dangling_class(_ID)).
checkont:-
        forall(check(C),
               forall(C,writeln(fail(C)))).

filter_class(ID,_S):-
        is_anonymous(ID).
%filter_relationship(_ID,T,_IDp):-
%        filter_relationship_type(T).
%filter_relationship_type(part_of).

% --QUERIES--

% TODO: generic query rewrites
% query(class(ID,N) where N like S)
% map generic query to a class query (defined in query_ontol)
amigo_query(term,Q,IDL):-
        lclass_query(Q,IDL).

rtypemap(transitive([]),['is a kind of (by transitivity)']).
rtypemap(transitive(P),Text):- rtypemap(t(P),Text).
rtypemap(t([]),['(by transitivity)']).
rtypemap(t([T|P]),[T,' '|L]):- rtypemap(t(P),L).

rtypemap(differentium_of,'differentiating characteristic of').
rtypemap(subclass,'is a kind of').
rtypemap(inverse(subclass),'has subtype').
rtypemap(part_of,'is part of').
rtypemap(inverse(part_of),'can have part').
rtypemap(develops_from,'develops from').
rtypemap(inverse(develops_from),'can develop into').
rtypemap(inverse(X),[X,'(inverse)']).
rtypemap(T,N):- property(T,N).
rtypemap(X,X).

id_filter_set(term,ID,SetID,SetN,ontology):-
        belongs(ID,SetID),
        ontology(SetID,SetN).

get_root(_,_,ID):- noparent(ID).
get_root(TSel,IDs,ID):-
        member(-ID,IDs),
        noparent_by_type1(TSel,ID).
get_root(TSel,IDs,ID):-
        member(ID,IDs),
        noparent_by_type1(TSel,ID).
noparent_by_type1(all,ID):- !,noparent(ID).
noparent_by_type1(T,ID):- noparent_by_type(T,ID).
% deprecated?
get_root(all,ID):- !, noparent(ID).
get_root(TSel,ID):- noparent_by_type(TSel,ID).

% (+,+,+,+) sd
% succeeds if relationship passes filter test
allowed_parent(all,ID,T,IDp):-
        !,
        (   restriction(ID,T,IDp),
%            is_transitive(T)
            true
        ;   subclass(ID,IDp),
            T=subclass).
allowed_parent(subclass,ID,subclass,PID):-
        !,
        subclass(ID,PID).
allowed_parent(T,ID,TLabel,IDp):-
        parent_over_nr(T,ID,IDp),
        (   parent(ID,T,IDp)
        ->  TLabel=b(T)
        ;   TLabel=i(T)).
allowed_parent_liberal(T,ID,TL,PID):-
        allowed_parent(T,ID,TL,PID).
allowed_parent_liberal(T,ID,TL,PID2):-
        T\=subclass,
        subclass(PID,PID2),
        allowed_parent(T,ID,TL,PID),
        TL\=subclass.

% ==================== VIEW ====================

% TERM RESULTS

search_result_headers(term,summary,
                      tr(th([]),th('ID'),th('Name'),th('Cross-Product'))).

search_result_id_view(term,summary,ID) =>
 if(config_setting(show_ids,false),
    then:[],
    else:td(ID)),
 td(href_id_as_label(term,ID)),
 td(onto_term_extra(ID)).

search_result_id_view(term,detail,ID) =>
 term_detail_panel(ID),
 term_lineage_boxes_panel(ID),
 term_relationship_summary(ID).

term_detail_panel(ID) =>
 doc:'widget showing summary details on class',
 call(belongs(ID,Ont)),
 div(class=block,id=info,
     h2('Term Information'),
     table(class=tagval_table,
           tdpair('ID',[ID,findall(Func,amigo_hook(term_extra_info(ID),Func))]),
           tdpair('Name',href_id_as_label(term,ID)),
           tdpair('Ontology',Ont),
           findall(tdpair('Definition',Def),
                   def(ID,Def)),                     
           if(genus(ID,_),
              tdpair('Logical Definition',logical_definition_panel(ID))),
           findall(tdpair('Synonym',[Synonym, i(' type:',b(T))]),
                   synonym(ID,T,Synonym)),
           findall(tdpair('Comment',Cm),
                   class_comment(ID,Cm)))).

logical_definition_panel(ID) =>
 doc:'genus-differentia',
 table(class=logical_definition,
       tdpair('Genus',div(class=genus,
                          href_id_as_label(term,Genus),
                          ' ',
                          NumClassWithSameGenus where setof_count(C,genus(C,Genus),NumClassWithSameGenus),
                          ' classes have this genus')),
       tdpair('Differentiating characteristic',
              div(class=differentium,
                  span(class=relation_subscript,
                       href_id_as_label(term,Slot)),
                  href_id_as_label(term,Val))) forall differentium(ID,Slot,Val))
 where genus(ID,Genus).

term_relationship_summary(ID) =>
 doc:'shows parents, children, etc',
 div(class=block,id=relationships,
     h3('Relations'),
     html:table(
                findall([relationship_table_row(ID,subclass,IDp),
                         ufindall(relationship_table_row(IDpT),
                                  subclassT(IDp,IDpT))],
                        subclass(ID,IDp)),
                findall(relationship_table_row(ID,T,IDp),
                        restriction(ID,T,IDp)),
                ufindall(relationship_table_row(ID,T,IDp,via,IDz),
                         (subclassT(ID,IDz),restriction(IDz,T,IDp)))),
     h3('Reciprocal relations'),
     html:table(
                findall(relationship_table_row(IDc,subclass,ID),
                        subclass(IDc,ID)),
                findall(relationship_table_row(IDc,T,ID),
                        restriction(IDc,T,ID)),
                if(setof(IDc-T,differentium(IDc,T,ID),IDcTs),
                   then:[tr(th('Used in logical definition of:')),
                         map(IDc-T,
                             relationship_table_row(IDc,T,ID),
                             IDcTs)]))
    ).


relationship_table_row(SID,T,OID) =>
 doc:'shows relationship triple',
 html:tr(td(href_id_as_label(term,SID)),
         td(nowrap=true,rtype(T)),
         td(href_id_as_label(term,OID))).

relationship_table_row(SID,T,OID,Via,ZID) =>
 doc:'shows relationship triple, derived via some class',
 html:tr(td(href_id_as_label(term,SID)),
         td(nowrap=true,rtype(T)),
         td(href_id_as_label(term,OID))),
 html:tr(td([]),
         td([]),
         td([rtype(Via),href_id_as_label(term,ZID)])).

relationship_table_row(OID) =>
 doc:'shows relationship triple, blank cell for subject',
      html:tr(td([]),
              td('(indirect)'),
              td(href_id_as_label(term,OID))).

detail_page(term) =>
 doc:'TOP: details on one (or more?) terms',
 session(S),
 call((getparam(S,id,ID),
       debug(amigo,'~w',id=ID),
       dgetparam(S,term_view,TermView,detail),
       debug(amigo,'~w',view=TermView),
       belongs(ID,Ont),
       debug(amigo,'~w',ont=Ont),
       class(ID,N))),
       outer(['OBD: Term report for ',ID,'-',N],
       div(class=main,
           basic_query_form,
           call(http_param_label(post_results,Submit)),
           div(class=contents,
               html:h2(span(class=term,N)),
               b('Show term in tree nav:'),
               ufindall(ilink(['[',TN,']'],
                              action=replace_tree,
                              show_relationship_type=T,
                              submit=Submit,id=ID,page=main),
                        (relationship_type_option(T),relname(T,TN))),
               br,
               'View:',
               findall(ilink([' [',TermViewOpt,'] '],
                             term_view=TermViewOpt),
                       option_term_view(TermViewOpt)),
               br,
               a(href='#data_section','See data attached to this term'),
               br,
               search_result_id_view(term,TermView,ID),
               a(name='data_section'),
               term_instances_by_ont(Ont)))).

rtype(T) =>
[
 doc:'shows a rel typ or its inverse',
 call(rtypemap(T,Text)),
 Text
].

onto_tree =>
 doc:'expandable tree view of ontology',
 html:div(class=tree,
          getparam(open_ids,OpenIDs),
          getparam(show_relationship_type,TSel,all),
          sform(dag,[],
                'Restrict to relation:',
                html:select(size=1,name=show_relationship_type,
                            findall(opt(show_relationship_type,T),
                                    relationship_type_option(T))),
                submit(redraw_dag),
                debug(amigo,'~w OpenIDs: ~w',[TSel,OpenIDs]),
                                %                      setof(onto_tree_node(root,ID,OpenIDs,TSel),
                                %                            get_root(TSel,ID))
                div(class=treeview,
                    onto_tree_node(root,ID,OpenIDs,TSel) forall_unique get_root(TSel,OpenIDs,ID))
               )).


onto_tree_node(T,ID,OpenIDs,TSel) =>
 doc:'expandable node in DAG plus recursive children',
 in(S,
    if(filter_class(ID,S),
       then:[],
       else:            
      html:ul(%class=treeview,
              html:li(if(member(ID,OpenIDs),
                         then:[ % ID has been explicitly opened
                                ilink(img_tree_down,close_id=ID),
                                dag_rel_term(T,ID),
                                ufindall(onto_tree_node(Tc,IDc,OpenIDs,TSel),
                                         allowed_parent_liberal(TSel,IDc,Tc,ID))
                              ],
                         else:[
                               if(\+(allowed_parent_liberal(TSel,_,_,ID)),
                                  then:img_tree_leaf,
                                  else:ilink(img_tree_right,open_id=ID)),
                               dag_rel_term(T,ID),
                               if(member(-ID,OpenIDs),
                                  then: % ID is in search path
                                 ufindall(onto_tree_node(Tc,IDc,OpenIDs,TSel),
                                          (   member(-IDc,OpenIDs),
                                              allowed_parent(TSel,IDc,Tc,ID))))
                              ])
                     )))).

dag_rel_term(T,ID) =>
 doc:'show term with parent rel type',
 session(S),
 font(size='-2',T),
 call(((   class(ID,N)
       ->  true
       ;   N='*unknown*'),
       ngetparam(S,highlighted_ids,IDhL),
       (member(ID,IDhL) -> Hi=1 ; Hi=0),
       (  get_app_param(S,show_ids,false)
       -> Text=N
       ;  Text=[ID,':',N]))),
 href_data_item(term,ID,Text,Hi),
 %onto_term_extra(ID),
 use_term_button(ID).

% SIMPLE WRAPPERS AND UTILS

attribute_value_text(A,V) =>
 doc:'shows A=V',
 html:font(size='-2',href_id_as_label(property,A)),
 if(class(V,_),
    then:href_id_as_label(term,V),
    else:data(V)).

onto_term_extra(ID) =>
 doc:'optional additional details; classdef',
 if(genus(ID,Genus),
    then:[
          html:b(' | '),
          html:font(size='-1',
                    href_id_as_label(term,Genus),',',
                    findall([html:font(size='-1',TN),href_id_as_label(term,V)],
                            (differentium(ID,T,V),relname(T,TN))))
         ],
    else:[]).

onto_term_name(ID) =>
 doc:'show term name only DEPREC',
 log(deprecated_otn),
 href_id_as_label(term,ID).

% TODO!! use amigo_property
href_id_as_label(property,ID) =>
 if(property(ID,N),
    then:[
          href_data_item(property,ID,N,0),
          use_term_button(ID)
         ],
    else: html:i(href_data_item(term,ID,ID))).

href_id_as_label(term,null) => [].
href_id_as_label(term,ID) =>
 doc:'show hyperlinked ID as label',
 if(class(ID,N),
    then:[
          if(is_anonymous(ID),
             then: html:i(href_data_item(term,ID,noesc(N))),
             else: href_data_item(term,ID,span(class=term,noesc(N)),0)),
          use_term_button(ID)
         ],
    else: html:i(href_data_item(term,ID,ID))).


wrap_onto_term(ID,Text,Hi) =>
 log(deprecated_wot),
 href_data_item(term,ID,Text,Hi).

term_instances_by_ont(Ont) =>
 doc:'the kind of instance report for a term depends on the ontology for that term',
 div(class=block,id=association,
     h2('Data and annotations'),
     if(data_class_by_ont(Ont,DataClass),
        then: search_results_box(DataClass))).

use_term_button(_ID) =>
 doc:'makes a button for the user to attach term TODO',
 [].

%            [call(js_use(ID,OnClick)),
%             input(type=button,
%                   onClick=OnClick,
%                   value='Use')]).

% (+,+,?) d
js_use(ID,OnClick):-
        belongs(ID,Ont),
        class(ID,N),
        ((Ont=attribute ; Ont=value)
        -> BoxName = Ont
        ;  BoxName = entity),
        concat_atom(['JavaScript:document.obd_query_basic.',
                     BoxName,
                     '.value=&quot;',
                     N,
                     '&quot;;'
                     ],
                    OnClick),
        !.
js_use(_,'').

       
           
% DAG

%%%%%%%%%%%%%%%%%%%%
% -- Outer templates --
%%%%%%%%%%%%%%%%%%%%

% each of these is a top-level call
main => page_dag.
entry_page(page_dag,'DAG').
page_dag =>
 doc:'TOP: Main OBD page: query box plus onto tree',
 outer('OBD',
       html:div(class=main,
                basic_query_form,
                                %qtable([[foo,data(S)]]),
                onto_tree)).

                  

                                % TODO
advanced_query =>
 doc:'TOP: Advanced Query Form TODO',
 outer('ODB - Advanced Query',
       html:div(class=main,
                basic_query_form,
                todo)).


% ==================== CONTROLLER ====================

%%%%%%%%%%%%%%%%%%%%
% -- Flow --
%%%%%%%%%%%%%%%%%%%%

post_results(S,A,IDL,IDopenL):-
        lgetparam(S,id,IDL),    % user-selected IDs
                                % (too big for fly - first path only?)
        debug(amigo,'search IDs: ~w',[IDL/S]),
                                % find all parents of IDs
        (   getparam(S,show_relationship_type,TSel)
        ->  debug(amigo,'restricted by type: ~w',[TSel]),
            setof(-IDp,ID^(member(ID,IDL),
                           parent_overRT(TSel,ID,IDp)),IDpL)
        ;   setof(-IDp,ID^(member(ID,IDL),
                           parentRT(ID,IDp)),IDpL)),
                                %           setof(-IDp,(member(ID,IDL),parentRT(ID,IDp)),IDpL),
        debug(amigo,'IDs to place in tree: ~w',[IDpL]),
        (   A=replace_tree
        ->  IDopenL=IDpL
        ;   (   getparam(S,open_ids,IDopenL1)
            ->  append(IDopenL1,IDpL,IDopenL)
            ;   IDopenL=IDpL)).

strans(main,S,
       % pre: list of term IDs selected, posted
       (   submit_param(S,post_results),
           getparam(S,data_class,term),
           getparam(S,action,A),
           (A=replace_tree ; A=plus_node)),
       % post:
       post_results(S,A,IDL,IDopenL),
       % change:
       add([[open_ids,IDopenL],[highlighted_ids,IDL]])).

strans(main,S,
       % pre: list of term IDs selected, posted
       (   submit_param(S,redraw_dag)),
       % post:
       post_results(S,replace_tree,IDL,IDopenL),
       % change:
       add([[open_ids,IDopenL],[highlighted_ids,IDL]])).

% DAG management
%  expand a node
strans(main,S,(lgetparam(S,open_id,IDL1),IDL1\=[]),
       (   (getparam(S,open_ids,IDL2)
           ->  append(IDL1,IDL2,IDL)
           ;   IDL=IDL1)),
       add([[open_ids,IDL]])).
%  collapse a node
transition(_X,main,S,S2):-
        lgetparam(S,close_id,IDL1),
        IDL1\=[],
        !,
        (getparam(S,open_ids,IDL2)
        ->  findall(ID,(member(ID,IDL2),not(member(ID,IDL1))),IDL)
        ;   IDL=[]),
        add_session_data(S,[[open_ids,IDL]],S2).


% detail report: for terms, we want to do an extra
% query.
% if amigo_instance is loaded, then data_class_by_ont may default to
% instance, and amigo_query(instance,class(ClassID),IDs) will be called
% TODO: separate widget for every dataclass; collapsable?
% supported dataclasses: term, instance, feature, mutant, ...
on_transition(detail_page,S,S2):-
        getparam(S,data_class,term),
        !,
        getparam(S,id,ID),      % ID of a particular data class
        belongs(ID,Ont),        % different onts may use different reports
        (   data_class_by_ont(Ont,DataClass)
        ->  debug(amigo,'~w',querying(ID)),
            amigo_query(DataClass,class(ID),IDL),
            add_session_data(S,[[ids,IDL]],S2),
            length(IDL,LenIDL),
            debug(amigo,'~w',queried_data_class(DataClass,class(ID),num=LenIDL))
        ;   S2=S).
