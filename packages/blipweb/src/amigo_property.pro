/* ***********************************************************

   amigo_property

   viewing and querying ontologies

   pages:

   main : basic DAG view
   search_property_results_main   
   property_report

   this plugin is extendable with other plugins

   when showing a property in detail, instances of that property/class
   will also be shown. the function used depends on the ontology
   and which plugins are present
   
************************************************************/

% BioProlog
:- use_module(bio(ontol_db)).
:- use_module(bio(query_ontol)).
:- use_module(bio(bioprolog_util)).

% AmiGO specific useful predicates
:- use_module(amigo_util).

% ==================== CONFIG ====================

% amigo params
data_class(property):- fail. % off by default

% this is overridden in components
data_class_by_ont(_Ont,_DataClass):- fail.
data_class_window_size(property,100).

header_tab(ilink('Property DAG', page=property_tree_main)):- data_class(property).

http_param_label(property,'properties / relationship types').

http_param_label(redraw_dag,'Redraw DAG').

% hide_attr(?Attr)
%  these attributes have special meaning in
%  the hi-level view, and should be shown in appropriate place

% ==================== MODEL ====================

%%%%%%%%%%%%%%%%%%%%
% -- Logic --
%%%%%%%%%%%%%%%%%%%%

% --QUERIES--

% map generic query to a class query (defined in query_ontol)
amigo_query(property,Q,IDL):-
        lproperty_query(Q,IDL).

lproperty_query(Q,IDL):-
        (setof(ID,property_query(Q,ID),IDL)
        ->  true
        ;   IDL=[]).
property_query(search(S),_):-
        var(S),
        !,
        throw(search_term_must_be_instantiated).
property_query(search(''),ID):-
        property(ID,_).
property_query(search(ID),ID):-
        ID \= '',
        property(ID,_).
property_query(search(S),ID):-
        S \= '',
        property(ID,S).
property_query(search(S),ID):-
        S \= '',
        downcase_atom(S,Slc),
        property_query(search_lc(Slc),ID).
property_query(search_lc(Slc),ID):-
        property(ID,N),
        downcase_atom(N,Nlc),
        sub_atom(Nlc,_,_,_,Slc).
property_query(search_lc(Slc),ID):-
        def(ID,N),
        downcase_atom(N,Nlc),
        sub_atom(Nlc,_,_,_,Slc).

id_filter_set(property,ID,SetID,SetN,ontology):-
        (belongs(ID,SetID) -> true ; SetID = null),
        ontology(SetID,SetN).

% ==================== VIEW ====================

% PROPERTY RESULTS

search_result_headers(property,summary,
                      tr(th([]),th('ID'),th('Name'),th('Cross-Product'))).

search_result_id_view(property,summary,ID) =>
 td(ID),
 td(href_id_as_label(property,ID)).

search_result_id_view(property,detail,ID) =>
 table(tdpair('ID',ID),
       tdpair('Name',href_id_as_label(property,ID)),
       findall(tdpair('Ontology',X),belongs(ID,X)),
       findall(tdpair('Comments',X),class_comment(ID,X)),
       findall(tdpair('Domain',href_id_as_label(term,X)),
               property_domain(ID,X)),
       findall(tdpair('Allowed values',href_id_as_label(term,X)),
               property_range(ID,X))).

tdpairs_by_pred(P,N,V) =>
 forall(P,tdpair(N,V)).

detail_page(property) =>
 doc:'TOP: details on one (or more?) propertys',
 session(S),
 call((getparam(S,id,ID),
       dgetparam(S,property_view,PropertyView,detail),
       property(ID,N))),
 outer(['OBD: Property report for ',ID,'-',N],
       html:div(class=main,
                basic_query_form,
                call(http_param_label(post_results,Submit)),
                html:div(class=property_info,
                         html:h2(N),
                         ilink('Show property in Tree-Browser',
                               action=replace_tree,
                               submit=Submit,id=ID,page=property_tree_main),
                         html:br,
                                %                           'View:',
                                %                           findall(ilink([' [',PropertyViewOpt,'] '],
                                %                                         property_view=PropertyViewOpt),
                                %                                   option_property_view(PropertyViewOpt)),
                                %                           html:br,
                         html:a(href='#data_section','See data attached to this property'),
                         html:br,
                         search_result_id_view(property,PropertyView,ID)))).

root_property(ID):-
        property(ID,_),
        not(subclass(ID,_)).

property_tree =>
 doc:'expandable tree view of ontology',
 html:div(class=tree,
          getparam(open_ids,OpenIDs),
          sform(dag,[],
                setof(property_tree_node(ID,OpenIDs),
                      memocall(root_property(ID))))).

property_tree_node(ID,OpenIDs) =>
 doc:'expandable node in DAG plus recursive children',
 html:ul(html:li(if(member(ID,OpenIDs),
                    then:[
                          ilink(img_tree_down,close_id=ID),dag_rel_property(ID),
                          findall(property_tree_node(IDc,OpenIDs),subclass(IDc,ID))
                         ],
                    else:[
                          if(nochild(ID),
                             then:img_tree_leaf,
                             else:ilink(img_tree_right,open_id=ID)),
                          dag_rel_property(ID)
                         ]))).

dag_rel_property(ID) =>
 doc:'show property with parent rel type',
 session(S),
 call((property(ID,N),
       ngetparam(S,highlighted_ids,IDhL),
       (member(ID,IDhL) -> Hi=1 ; Hi=0),
       (   get_app_param(S,show_ids,false)
       ->  Text=N
       ;   Text=[ID,':',N]))),
 href_data_item(property,ID,Text,Hi).

href_id_as_label(property,ID) =>
 doc:'show hyperlinked ID as label',
 if(class(ID,N),
    then:[
          if(is_anonymous(ID),
             then: html:i(href_data_item(property,ID,N)),
             else: href_data_item(property,ID,N,0)),
          use_property_button(ID)
         ],
    else: html:i(href_data_item(property,ID,ID))).
       
           
% DAG

%%%%%%%%%%%%%%%%%%%%
% -- Outer templates --
%%%%%%%%%%%%%%%%%%%%

% each of these is a top-level call
property_tree_main =>
 doc:'TOP: Main OBD page: query box plus onto tree',
 outer('OBD',
       html:div(class=main,
                basic_query_form,
                                %qtable([[foo,data(S)]]),
                property_tree)).

% ==================== CONTROLLER ====================

% todo - make more generic with terms

% DAG management
xxtransition(_X,property_tree_main,S,S2):-
        lgetparam(S,open_id,IDL1),
        IDL1\=[],
        !,
        (getparam(S,open_ids,IDL2)
        ->  append(IDL1,IDL2,IDL)
        ;   IDL=IDL1),
        add_session_data(S,[[open_ids,IDL]],S2).
xxtransition(_X,property_tree_main,S,S2):-
        lgetparam(S,close_id,IDL1),
        IDL1\=[],
        !,
        (getparam(S,open_ids,IDL2)
        ->  findall(ID,(member(ID,IDL2),not(member(ID,IDL1))),IDL)
        ;   IDL=[]),
        add_session_data(S,[[open_ids,IDL]],S2).

