/* ***********************************************************

   amigo_core

   these are the core components for an amigo-style display. amigo_core
   will not do much in itself - you should also use this in conjunction
   with other amigo views/plugins:

   -amigo_term : querying ontologies
   -amigo_feature : wild-type features (genes, gene products)
   -amigo_mutant : genotypes and phenotypes
   -amigo_xp : (experimental) stuff for cross-products

   all views depend on the term view

   generally there are 3 types of pages:

   -main_page - with exploratory DAG browser
   -search_results_page - showing chunks of results; default summary view
   -detail_page - showing a result in detailed view

   the main page is simply called 'main'

   the search_results_page is modified by two params: view and filter

   the user can filter the chunked results using a tabbed filter;
   each data_class can be filtered differently, see id_filter_set/5

   the view param shows different views of the data: summary, detail etc

   detail_page really just delegates to a detail view
   ; it delegates first to detail_page(DataClass) which is handled
   in the appropriate plugin
   
   generally each report is independent; however, a term (class)
   report may show instances of that class, according to what plugins
   are available and what the ontology is

   each plugin should define the PREDICATE search_result_headers/3
   - this is the list of table headers for summary views
   
   each plugin should also define
   - sdefun(search_result_id_view(DataClass,View,ID)
   for all classes and their views

   if the plugin is defining some kind of instance view, it should also
   define
   
   todo:
   -sort
   -qbuilder

   also?
   -organism
   
   we probably want a feature report as template but not
   actually seen

   best to customize query results

   general form:

   ID name (gene) (org) Role
                        Class  aV
                               aV

   specific form:

   MUTANTS/VARIANTS

   ID Name Gene Org Phenotype
                    C (at stage)
                      aV
                      aV

   GENE PRODUCT

   ID Name Org Role
               Class (aV) evidence
                     (aV)

   GENE AND PHENOTYPE

   ID Name Org Role                 Phenotype
               Class (aV) evidence  C aV
                     (aV)

   GENE AND MUTANT

   ID Name Org Role  Mutant
               C aV  ID Name Phen
                             
   
   each data class can be split into separate module

   diff display and qbuilder
   
   can this be abstracted into config?

   separate experimental module for slot views on GO
   
   
************************************************************/

:- use_module(bio(serval)).
:- use_module(bio(bioprolog_util)).
:- use_module(bio(mode)).
:- use_module(bio(dbmeta)).

% AmiGO specific useful predicates
:- use_module(amigo_util).

% ==================== CONFIG ====================

:- multifile
        entry_page/2,
        amigo_query/3,
        amigo_hook/2,
        send_option/2,
        data_class/1,
        data_class_by_ont/2,
        data_class_window_size/2,
        search_result_headers/3,
        header_tab/1,
        local_css/1,
        amigo_url/2,
        app_title/1,
        app_logo/1,
        amigo_param/2,
        default_user_setting/2,
        id_filter_set/5,
        option_term_view/1.
:- dynamic amigo_dynamic_param/2.

user:app_title(title):- fail.
user:app_logo(logo):- fail.
user:default_user_setting(param,val):- fail.

user:amigo_param(P,V):- amigo_dynamic_param(P,V).

header_tab(ilink(Title,page=Page)):-
        entry_page(Page,Title).

% serval params
app_name(App):-
        amigo_param(app_name,AppLocal),
        !,
        concat_atom(['/',amigo,'/',AppLocal],App).
app_name(amigo).
form_method('GET').
%init_page(main).
%data_class(all_data_class).

config_desc(show_ids,'True if term IDs are to be show together with names').
config_type(show_ids,boolean).

config_desc(logo,'Logo Image File').
config_type(logo,text).
config_global_default(logo,'amigo_logo.png').

% used? redundant with label?
http_param_desc(id,'The entity ID for a detailed view').
http_param_desc(data_class,'the category of entity: term, feature, mutant').
http_param_desc(page,'serval paaram: the current page').
http_param_desc(goto,'a directive indicating a state change to a page').

hidden_param(id).
hidden_param(data_class).       % term/feature/mutant etc
%hidden_param(page).             % current page sfunc   TODO: TEST
hidden_param(filter).           % additional ID constraint; this needs hidden otherwise not sticky between scroll selections

% standard views of data
%  different plugins may add to this
data_view(detail).
data_view(summary).

% user select option for what to do with selected IDs
send_option(_,Opt):- data_view(Opt).

% not necessarily a param - in fact, usually a param value...
%http_param_label(all_data_class,'entire database').
http_param_label(detail,'Detailed View').
http_param_label(summary,'Summary View').
http_param_label(post_results,'Show Me'). % do something with selected IDs
http_param_label(search_data,'Go').  % basic search form actio
http_param_label(load_bioresources,'Load Resources').
http_param_label(unload_bioresources,'Unload Resources').

base_url(URL):- amigo_param(base_url,URL),!.
base_url('http://127.0.0.1').
%base_url('http://yuri.lbl.gov').

% todo: make more generic
:- multifile www_document_root/1.
default_www_document_root('/Library/WebServer/Documents').
get_www_document_root(P):- www_document_root(P),!.
get_www_document_root(P):- default_www_document_root(P),!.

make_url(Path,URL):- base_url(BaseURL),concat_atom([BaseURL,Path],'/',URL).

amigo_url(css,URL):- amigo_param(base_url_css,URL),!.
amigo_url(css,URL):- make_url('amigo2/css',URL).

amigo_url(images,URL):- make_url('amigo2/images',URL).
amigo_logo_url(URL):-
        config_setting(logo,File),
        atom_concat('amigo2/images/',File,Path),make_url(Path,URL).

amigo_hook(_,_):- fail.

local_css('details_layout.css').
local_css('formatting.css').
local_css('obd.css').
local_css('main_layout.css').

% see http://openrico.org/rico/downloads.page
local_ajax('prototype.js').
%local_ajax('rico.js').
local_ajax('blip_amigo.js'). % define as part of blip - see css dir
local_ajax('sorttable_basic.js'). % define as part of blip - see css dir
%local_ajax('drivetip.js').


amigo_css(CSS):-
        amigo_url(css,URL),
        local_css(Local),
        concat_atom([URL,Local],'/',CSS).

amigo_ajax(Src):-
        amigo_url(css,URL),
        local_ajax(Local),
        concat_atom([URL,Local],'/',Src).

%%%%%%%%%
% LOGIC %
%%%%%%%%%

% filter tabs

% id_filter_set/5 should be defined in plugin
%  it defines the broad category with which an ID can be grouped
id_filter_set(DataClass,ID,Set):-
        id_filter_set(DataClass,ID,Set,_,_).
id_filter_set(DataClass,ID,Set,SetN):-
        id_filter_set(DataClass,ID,Set,SetN,_).
ids_in_set(DataClass,IDL,Set,IDsInSet):-
        length(IDL,LenIDL),
        userlog(num_ids=LenIDL),
        setof(ID,(member(ID,IDL),
                  id_filter_set(DataClass,ID,Set)),
              IDsInSet).

% (+,+,?)
%  each ID from each category can be filtered into broad sets
%  this predicate unifies with such a set list
%  IDsets = [idset(SetName,IDList,NumberOfIDsInSet),...]
partition_ids(DataClass,IDL,IDSets):-
        setof(mem(ID,SetID,SetN),
              ID^(member(ID,IDL),id_filter_set(DataClass,ID,SetID,SetN)),
              MemL),
        setof(SetID-SetN,ID^member(mem(ID,SetID,SetN),MemL),Sets),
        setof(idset(SetID,SetN,IDsInSet,Num),
              (   member(SetID-SetN,Sets),
                  setof(ID,member(mem(ID,SetID,SetN),MemL),IDsInSet),
                  length(IDsInSet,Num)),
              IDSets).

% MODEL

/**
  @pred generic_inst_sv(+InstID,?AttrID,?Value) nd
   filters out special-meaning attributes
*/
generic_inst_sv(ID,A,V):-
        inst_sv(ID,A,V),
        not(hide_attr(A)).

% role_attr(+Attr)
%  unifies with attributes for linking features with their role
%  TODO: replace with subproperties?
role_attr(has_role).
role_attr(has_phenotype).


/**
  @pred instance_evidence(+InstanceID,?EvidenceID) nd
   some instances have an evidence code supporting their
  existence
*/
instance_evidence(ID,EvID):-
        inst_sv(ID,with_evidence,EvID).

organism(ID,N,N2):-
        inst_of(ID,organism),
        inst(ID,N2),
        inst_sv(ID,scientific_name,N).



/**
  @pred instRTA(?InstID,+ParentClassID) nondet
  
  finds instances of ParentClassID. uses parentRT, so:
  if I inst-of C' and C' part-of C, then I instRTA C

  if InstID is a xp-inst (inst_sv is filled) then xps are checked
*/
% parentRT(?,+) is expensive, so best to iterate over inst
% then do parentRT(+,+)
instRTA(InstID,ParentClassID):-
        inst_of(InstID,ClassID),   % (?,+) => (+,+) nd
        parentRT(ClassID,ParentClassID). % (+,+)
instRTA(InstID,ParentClassID):-
        genus(ParentClassID,ParentGenusID), % (?,?) => (+,+)
        inst_of(InstID,GenusID),   % (?,+) => (+,+)
        subclassRT(GenusID,ParentGenusID), % (?,+) => (+,+)
        setof(Slot:Val,inst_sv(InstID,Slot,Val),SVL),
        setof(Slot:Val,differentium(ParentClassID,Slot,Val),ParentSVL),
        scDL(SVL,ParentSVL).
                                %writeln(rta(SVL,ParentSVL,InstID,ParentClassID,GenusID,ParentGenusID)).

test_case(not(instRTA('FB:FBgn0000054','GO:0007629'))).
test_case(instRTA('FB:FBgn0000173','GO:0007629')).

% TODO: not recursive
scDL([],[]).
scDL([SV|SVL],ParentSVL):-
        SV=S:V,
        (select(S:ParentV,ParentSVL,RestParentSVL) % TODO: subproperty
        ->  subclassRT(V,ParentV), % shared slot; check slot parentage
            scDL(SVL,RestParentSVL)
        ;   scDL(SVL,ParentSVL)). % child has more specific slot


% combined query
% each ID result is actually a Categ-ID pair
amigo_query(all_data_class,Q,IDL):-
        findall(pair(Categ,ID),
                (   data_class(Categ),
                    Categ\=all_data_class,
                    amigo_query(Categ,Q,IDs1),
                    member(ID,IDs1)),
                IDL).
id_filter_set(all_data_class,pair(Categ,_ID),Categ,Categ,all).

pair(Categ,ID) => join('-',[Categ,ID]).

search_result_id_view(all_data_class,summary,pair(Categ,ID)) =>
 td(b(Categ)),
 search_result_id_view(Categ,summary,ID).

href_id_as_label(xref,ID-Label) =>
 doc:'generic database cross-ref. Should have GO.xrf_abbs loaded, as obo file',
 if( (concat_atom([DB,Local],':',ID),inst_sv(DB,'GOMetaModel:url_syntax',BaseUrl),atom_concat(BaseUrl,Local,Url)),
     then: a(href=Url,Label),
     else: Label).
href_id_as_label(xref,ID) =>
 doc:'generic database cross-ref. Should have GO.xrf_abbs loaded, as obo file',
 if( (concat_atom([DB,Local],':',ID),inst_sv(DB,'GOMetaModel:url_syntax',BaseUrl),atom_concat(BaseUrl,Local,Url)),
     then: a(href=Url,ID),
     else: ID).

% ==================== VIEW ====================

%%%%%%%%%%%%%%%%%%%%%%%%%
% -- Basic Widget Set --
%%%%%%%%%%%%%%%%%%%%%%%%%
amigo_img(LocalURL,Name) =>
 doc:'img tag',
 amigo_img(LocalURL,Name,[]).
amigo_img(LocalURL,Name,AL) =>
 doc:'img tag',
 [call((amigo_url(images,URLDir),
        concat_atom([URLDir,LocalURL],'/',URL),
        Elt =.. [img,src=URL,text=Name|AL])),
  Elt].
textfield(X) =>
 doc:'basic textfield',
 html:input(type=textfield,name=X).
submit(N) =>
 doc:'basic submit button',
 call(http_param_label(N,Text)),
 html:input(name=submit,type=submit,value=Text).
opt(N) =>
 doc:'option - map name to user text',
 call((http_param_label(N,Text) ; Text=N)),
 html:option(value=N,Text).
opt(Param,N) =>
 doc:'map name to user text; check for selected',
 session(S),
 call((http_param_label(N,Text) ; Text=N)),
 if(ngetparam(S,Param,N),
    then: html:option(value=N,selected=on,Text),
    else: html:option(value=N,Text)).
cond_highlight(C,Out) =>
 doc:'conditionally highlight output',
 if(C,then: html:b(Out),else: Out).
tagval(Tag,Val) =>
 doc:'amigo style tag:val in ul list',
 html:li(html:span(class=info_label,Tag),
         html:span(Val)).
dlpair(Tag,Val) =>
 doc:'amigo style tag:val in dl list',
 dt(Tag),
 dd(Val).
tdpair(Tag,Val) =>
 doc:'amigo style tag:val in table list',
 html:tr(html:th(Tag),html:td(Val)).
ntable(Name,Rows) =>
 doc:'named table for tag:val tdpairs',
 html:b(Name),
 html:table(class=summary_table,Rows).
tvgroup(Name,TagVar,ValVar,Call) =>
 doc:'summarises list of tag:val pairs',
 call(xsetof(TagVar:ValVar,Call,TagVals)),
 if(setof(Tag,V^member(Tag:V,TagVals),Tags),
    html:ul(class=tvgroup,
            html:li(class=info_label,Name),
            map(Tag,
                html:ul(html:li(Tag),
                        html:ul(findall(html:li(Val),
                                        member(Tag:Val,TagVals)))),
                Tags))).
lrpanel(C1,C2) =>
 doc:'2 column table: two panels, left and right',
 html:table(html:tr(html:td(C1),html:td(C2))).
checkbox(Name,Val) => checkbox(Name,Val,(1=0)).
checkbox(Name,Val,Expr) =>
 if(Expr,
    then: html:input(type=checkbox,name=Name,value=Val,checked=on),
    else: html:input(type=checkbox,name=Name,value=Val)).
jscheckbox(Name,Val,Expr) =>
 if(Expr,then:Checked=1,else:Checked=0),
 html:input(type=checkbox,
            name=Name,
            value=Val,
            onClick='submit()',
            checked=Checked).
href_data_item(DataClass,ID,Text) =>
 doc:'default',
 href_data_item(DataClass,ID,Text,0).
href_data_item(DataClass,ID,Text,Hi) =>
 doc:'show a data item with text and link to report',
 ilink(if(Hi=0,
          then: Text,
          else: html:font(color=red,Text)),
       goto=detail_page,data_class=DataClass,id=ID).
localhref(ID,N) =>
 call(atom_concat('#',ID,URL)),
 a(href=URL,N).


img_tree_down =>
 doc:'glyph for showing contents and closing node',
 amigo_img('tree-down.gif','Close Node').
img_tree_right =>
 doc:'glyph for showing node has contents and opening node',
 amigo_img('tree-right.gif','Close Node').
img_tree_leaf =>
 doc:'glyph for leaf node - actually a blank space',
 amigo_img('1pix.gif','Leaf Node',[width=10,height=1]).


%%%%%%%%%%%%%%%%%%%%
% -- Outer templates --
%%%%%%%%%%%%%%%%%%%%

% each of these is a top-level call

search_results_page =>
 doc:'TOP: after user performs basic search, show results matching constraints',
 getparam(data_class,DataClass),
 outer('OBD - Search results',
       html:div(class=main,
                basic_query_form,
                search_results_box(DataClass))).

detail_page =>
 doc:'TOP: when a user clicks on a single ID, they go to this page',
 getparam(data_class,DataClass),
 detail_page(DataClass).        % defined in plugin

entry_page(dbstats,'DB Stats').
dbstats =>
 doc:'TOP: show statistics',
 outer('DB Statistics',
       html:div(class=stats,
                basic_query_form,
                call((   findall(Schema-Stat,dbmeta:schema_statistic(Schema,Stat),SchemaStats),
                         solutions(Schema,member(Schema-_,SchemaStats),Schemas))),
                dbstats(Schema,SchemaStats) forall member(Schema,Schemas))).
dbstats(Schema,SchemaStats) =>
  div(class=stats_by_schema,
      h3(Schema),
      dl(class='general-info',
         dlpair(RU,Num) forall member(Schema-count(RU,Num),SchemaStats)),
      dl(class='general-info',
         dlpair(RU,
                dl(class='general-info',
                   dlpair(FilterBy,Num) forall member(FilterBy-Num,Pairs)))
        forall member(Schema-count_by(RU,Pairs),SchemaStats))).

entry_page(preferences,'Preferences').
preferences =>
 doc:'TOP: edit preferences',
 outer('Preferences',
       div(class=main,
           basic_query_form,
    widget_preferences)).

widget_preferences =>
 doc:'generic box for editing preferences',
 sform(preferences,[page],
       table(tr(th('Preferences')),
             findall(tr(td(Name),
                        td(if(config_type(Name,Type),
                              then:
                             widget_preferences_config_option(Name,Val,Type),
                              else:
                             Val)),
                        td(Desc)),
                     (   config_desc(Name,Desc),
                         config_setting(Name,Val))))
       %widget_load_bioresource
      ).

widget_preferences_config_option(Name,_,enum(Enum)) =>
 select(size=1,name=Name,
        onChange='submit(\'foo\')',
        map(Opt,
            html:option(value=Opt,Opt),
            Enum)).
widget_preferences_config_option(Name,_,boolean) =>
 input(type=checkbox,
       name=Name,
       onClick='submit()').
widget_preferences_config_option(Name,Value,text) =>
 input(type=text,
       name=Name,
       value=Value).

widget_load_bioresource =>
 doc:'show list of bioresources',
 sform(bioresources,[page],
       table(tr(th(''),th('Resource'),th('Status')),
             tr(td(checkbox(bioresource,noesc(R))),
                        td(noesc(R)),
                '') forall bioresource(R,_,obo)),
       submit(load_bioresources)).
 

page_admin =>
 comment('administration - what resources are loaded etc'),
 div(class=admin,
     h3('Bioresources loaded into server:'),
     table(findall(tr(td(input(type=checkbox,
                               name=RName,
                               onClick='submit()')),
                      td(RName)),
                   bioresource(RName)))).
    
    
about_header =>
 h2('Amigo next-generation demo').

about_footer =>
 br,p('This is demo software: data may not be accurate').
generic_about_message =>
 p('There is no info on this particular AmiGO application').

entry_page(about,'About').
about =>
 doc:'TOP: about the current application and ontologies',
 outer('ODB - DB Statistics',
       html:div(class=main,
                basic_query_form,
                about_header,
                html:div(class=about,
                         if(amigo_param(about,About),
                            then: About,
                            else: generic_about_message)),
                html:br,
                p('This application uses the following resources:'),
                table(tr(th('Ontology'),th('Name'),th('Description')),
                      findall(tr(td(ID),td(Name),td(Desc)),
                              ontology(ID,Name,Desc))),
                about_footer)).

userstats =>
 doc:'TOP: show user statistics',
 outer('ODB - User Statistics',
       html:div(class=main,
                basic_query_form,
                table(tr(th('Session ID'),th('Stats')),
                      findall(tr(td(ID),td(data(Stats))),
                              serval:session_stats(ID,Stats))))).

sessiondata =>
 doc:'TOP: show session data',
 outer('ODB - User Statistics',
       html:div(class=main,
                basic_query_form,
                call(serval:next_session_id(SID)),
                ul(tagval(next_session_id,SID)),
                table(tr(th('Session ID'),th('Param'),th('Value')),
                      findall(tr(td(ID),td(data(Param)),td(data(Value))),
                              serval:session_data(ID,Param,Value))))).

%%%%%%%%%%%%%%%%%%%%
% -- Diagnostic --
%%%%%%%%%%%%%%%%%%%%
no_matches(DataClass) =>
 doc:'no matches for query; offer search on other class',
 sform(anon,[data_class],
       session(S),
       call(http_param_label(DataClass,N)),
       call(ngetparam(S,search_text,SearchText)),
       'No matches found for ',
       html:i(SearchText),
       ' in data section:',
       html:b(N),
       html:br,
       html:input(type=hidden,name=search_text,value=SearchText),
       table(tr(td('Try another search, this time in'),
                td(select(size=1,name=data_class,
                          findall(opt(C),(data_class(C),C\=DataClass)))),
                td(submit(search_data))))).

%%%%%%%%%%%%%%%%%%%%
% -- Main Widgets --
%%%%%%%%%%%%%%%%%%%%
basic_query_form =>
 doc:'Mini-Constraints box for querying data of all classes',
 html:div(%style='display: none;',
          id='menuSearchForm',
          class='toggleable',
          sform(obd_query_basic,[data_class],
                table(tr(td('Search for text:'),
                         td(input(id='bar-query',class=txt,
                                  type=textfield,name=search_text)),
                         td(in),
                         td(html:select(size=1,name=data_class,
                                        findall(opt(C),
                                                data_class(C)))),
                         %td(input(value='Ajax Search',onclick='javascript:doAmigoSearch()',type='button')),
                         td(submit(search_data))))),
          query_builder_box
         %ajax_preseeded_table
         ).

ajax_preseeded_table =>
 div(id=data_grid_container,
     div(id=data_grid_viewport,
         table(id=data_grid,findall(tr(th(X),
                                       th(y),
                                       th(a),
                                       th(b),
                                       th(c)),
                                    member(X,[1,2,3,4,5,6,7,8,9,10]))))).


query_builder_box =>
 doc:'Shows current query construction TODO',
 in(S,
    if(getparam(S,current_query,Q),
       html:div(class=query_builder,
                sform(query_builder,[],
                      table(tr(td(query),td(Q))))))).


/*
  a collection of IDs can be summarised in a 'box'. the summary is
  defined by the sfunc search_results_table/3 which should be
  defined in the plugin. this function should show a checkbox next to
  each ID. Different data classes may have different actions
  that can be perfomed on them (eg terms can be appended to or used
  to create new trees). this is defined by result_selector, also
  defined by the plugin

  TODO: some kind of notion of extension??
  allow plugins to override behaviour?
  
*/
xxxxsdefun(search_results_box(all_data_class)
      /'show results for each class separately',
      html:div(class=results,
               getparam(ids,IDL),
               call(data_class_window_size(DataClass,WS)->true ; WS=50),
               if(IDL=[],
                  then: no_matches(DataClass),
                  else:[
                        search_results_filter_tab(DataClass,IDL,IDLf),
                        search_results_subselect(IDLf,IDLs,From,WS,DataClass),
                        sform(list,[action,id],
                              input_hidden(data_class,DataClass),
                              search_results_table(DataClass,IDLs,From),
                              result_selector(DataClass))
                       ]))).
% this is overridden by widget
search_results_box(DataClass) =>
 doc:'Summary of all results matching query; requires p:ids',
 html:div(class=results,
          getparam(ids,IDL),
          call(data_class_window_size(DataClass,WS)->true ; WS=50),
          if(IDL=[],
             then: no_matches(DataClass),
             else:[
                   search_results_filter_tab(DataClass,IDL,IDLf),
                   search_results_subselect(IDLf,IDLs,From,WS,DataClass),
                   sform(list,[action,id],
                         input_hidden(data_class,DataClass),
                         search_results_table(DataClass,IDLs,From),
                         result_selector(DataClass))
                  ])).

result_selector(DataClass) =>
 doc:'selects what to do with query results',
 html:select(size=1,name=action,
             findall(opt(X),
                     send_option(DataClass,X))),
 html:input(type=button,
            onClick='JavaScript:check(document.forms[&quot;feature_list&quot;].feature_id);',
            value='Check/Uncheck All'),
 submit(post_results).

search_results_table(DataClass,IDL,From) =>
 doc:comments('Summary table: a subset of results matching query',
              'The IDs should already have been chunked into an',
              'easily displayed set. Each ID is numbered and a ',
              'checkbox added'),
 table(id=data_grid,
       %class=results,
       class=fixedTable,
       getparam(view,View,summary),
       if(search_result_headers(DataClass,View,HeaderTR), % in plugin
          HeaderTR),
       nmap(ID,
            IDnumber,
            tr(td(html:input(type=checkbox,
                             name=id,value=ID),  % check for E-A
                  IDnumber,':'),
               search_result_id_view(DataClass,View,ID)),
            IDL,
            From)).

api_data_grid =>
 doc:'returns XML with embedded tr/td tags for use by ajax client',
 getparam(data_class,DataClass), 
 getparam(search_text,Text),
 debug(amigo,'query type=~w search="~w"',[DataClass,Text]),
 call(amigo_query(DataClass,search(Text),IDs)),
 search_results_ajax_xml(DataClass,IDs).

search_results_ajax_xml(DataClass,IDL) =>
 doc:comments('Summary table in XML as required by rico'),
 html:'ajax-response'(html:response(type=object,
                                    id=data_grid_updater,
                                    html:rows(update_ui=true,
                                              getparam(view,View,summary),
                                              nmap(ID,
                                                   IDnumber,
                                                   tr(td(html:input(type=checkbox,
                                                                    name=id,value=ID),
                                                         IDnumber,':'),
                                                      search_result_id_view(DataClass,View,ID)),
                                                   IDL,
                                                   1)))).

search_results_filter_tab(DataClass,IDL,IDLf) =>
 doc:comments('tabbed form for filtering search results',
              'the full ID list is partitioned into sets, which',
              'can then be used as filters'),
 session(S),
 call(((partition_ids(DataClass,IDL,IDsets) -> true ; IDsets=[]),
       dgetparam(S,filter,Filter,'All'),
       ((member(idset(Filter,_FilterN,IDLf,_),IDsets),Filter\='All')
       ->  true
       ;   IDLf=IDL),
       length(IDL,Total),
       IDsets2=[idset('All','All',[],Total)|IDsets])),
 ul(class=filter_tab,
    map(idset(Group,GroupN,_,GroupIDcount),
        [call((Group=Filter
              -> TDStyle=active_filter_tab
              ;  TDStyle=inactive_filter_tab)),
         li(class=TDStyle,
            ilink([GroupN,':',GroupIDcount],filter=Group))
        ],
        IDsets2)).

% (+,?,?,+,+)
search_results_subselect(IDLin,IDLout,Pos,ListWinSize,_WText) =>
 doc:'generic results screen-scroller: shows results in windowed chunks',
 call(length(IDLin,NumID)),
        %html:b(['Total ',WText,': ']),
        %data(NumID),
        %html:br,
 if(NumID =< ListWinSize,
    then: call((IDLout = IDLin, Pos=1)),
    else:[
          session(S),
          call((getparam_as_num(S,list_position,Pos) ; Pos=1)),
          call(Pos0 is Pos-1),
          call(splicelist(IDLin,Pos0,ListWinSize,IDLout,_)),
          call(findall(X,
                       (
                         list_bin(NumID,ListWinSize,Beg,End),
                         Text=b([Beg,'..',End]),
                         (Beg=Pos
                         ->  X=Text
                         ;   X=ilink(Text,list_position=Beg))
                       ),XL)),
          data('Showing:'),
          join(' | ',XL)
         ]).

report_by_data_class(DataClass) =>
 doc:'show a single instance ID',
 getparam(id,ID),
 table(tr(search_result_id_view(DataClass,detail,ID))).

% GENERIC

% -- END OF RESULTS

page_head_element(N) =>
  html:head(html:title(N),
            include_file(amigo_conf('js_checkbox_select.html')),
            html:meta(name=description,
                      content='amigo2 prototype'),
            html:meta(name=robots,
                      content='index,nofollow,NOARCHIVE'),
            html:meta(name=author,
                      content='Chris Mungall'),
            findall(html:script(type='text/javascript',
                                src=AjaxSrc),
                    amigo_ajax(AjaxSrc)),
            findall(html:link(href=CSS,
                              rel=stylesheet,
                                        type='text/css'),
                    amigo_css(CSS))).


% TEMPLATE
outer(N,P) =>
 doc:'wraps HTML in main template; this is the main look and feel template',
 html:html(                     %call(amigo_logo_url(LogoURL)),
           %call(findall(X,header_tab(X),Headers)),
           page_head_element(N),
           html:body(onload='javascript:bodyOnLoad()',
                     div(id=header,
                         a(class='logo floatR',
                           href=App,
                           img(src=LogoURL,alt='logo') where amigo_logo_url(LogoURL)) where app_name(App),
                         h1(id=top,Title) where app_title(Title)),
                     div(id=searchbar,
                         ul(id=menuToggle,
                            li(ilink(LinkTitle,page=LinkPage) forall entry_page(LinkPage,LinkTitle)))),
                     html:div(id=center,P),
                     
                     html:hr,
                     html:div(class=footer,
                              html:font(size='-2','-- This is DEMO software --'))
                    )).

%%%%%%%%%%%%%%%%%%%%
% -- Rico --
%%%%%%%%%%%%%%%%%%%%

% remember: response.setHeader("Content-Type", "text/xml");
data_grid_ajax_response(ID,Tbl) =>
 call(atom_concat(ID,'_updater',UID)),
 ajax_response(response,type=object,id=UID,
               rows(update_ui=true,
                    map(Row,tr(map(Cell,td(Cell),Row)),Tbl))).

% ==================== CONTROLLER ====================

%%%%%%%%%%%%%%%%%%%%
% -- Flow --
%%%%%%%%%%%%%%%%%%%%

% search results for various data classes
transition(_,search_results_page,S,S2):-
        submit_param(S,search_data),
        getparam(S,data_class,DataClass),
        !,
        ngetparam(S,search_text,Text),
        debug(amigo,'query type=~w search="~w"',[DataClass,Text]),
        (   amigo_query(DataClass,search(Text),IDL)
        ->  true,
            length(IDL,LenIDL),
            debug(amigo,'found ~w ids',[LenIDL])
        ;   throw(no_query_defined_for(DataClass))),
        % we add all ids to session data - this means that
        % the ID list isn't associated directly w the page;
        % also can be memory intensive.
        % the alternative is to preserve the query and redo any time
        % the id list is needed (eg in windowing)
        % OR do the windowing on the client (Ajax/JS)
        add_session_data(S,[[ids,IDL],[view,summary]],S2).

% user selects IDs with checkbox, then chooses action
% (eg detail view)
transition(_,search_results_page,S,S2):-
        submit_param(S,post_results),
        getparam(S,action,View),
        data_view(View),
        !,
        lgetparam(S,id,IDL),
        % todo - a better way of unsetting a param
        add_session_data(S,[[ids,IDL],[id],[view,View]],S2).

% goto
transition(_,P,S,S):-
        getparam(S,goto,P),
        !.

strans(preferences,S,
       % pre:
       submit_param(S,load_bioresources),
       % post:
       (   lgetparam(S,bioresource,Rs),
           maplist(load_bioresource,Rs)),
       % add:
       add([])).
strans(preferences,S,
       % pre:
       submit_param(S,unload_bioresources),
       % post:
       (   lgetparam(S,bioresource,Rs),
           maplist(unload_bioresource,Rs)),
       % add:
       add([])).

strans(api_data_grid,S,
       % pre:
       getparam(S,api,data_grid),
       % post:
       userlog(data_grid),
       add([])).
